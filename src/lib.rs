//! A graphics library for WebAssembly that is mostly compatible with OpenGL ES 2.0/3.0.
//!
//! # Usage
//!
//! Currently, `aureum` can only be used in Rust, and must be used with an OpenGL loading library,
//! such as the [`gl` crate][gl]. Static bindings and a C library are planned for future releases.
//!
//! In your Rust project, add the latest versions of `aureum` and `gl` to your `[dependencies]`,
//! and then create a context and load the pointers for `aureum` before calling into GL. For
//! example, your `main.rs` may look like this:
//!
//! ```no_run
//! fn main() {
//!     // non-GL code may go here ...
//!
//!     gl::load_with(aureum::get_proc_address);
//!     let context = ContextBuilder::new()
//!         .gl_version(Api::Gles, (2, 0)) // specify the required API version
//!         .canvas_id("app") // tell aureum to use the canvas with `id="app"`
//!         .build()
//!         .expect("failed to build context");
//!     context.make_current();
//!
//!     // ... GL code must go here, after the pointers are loaded
//! }
//! ```
//!
//! Context creation and function pointer loading may be deferred as long as you like, as long as
//! they both occur before any GL commands are invoked. You may even abstract them using a
//! higher-level API like Piston if you wish.
//!
//! [gl]: https://crates.io/crates/gl

use std::borrow::Cow;
use std::cell::{Ref, RefCell};
use std::collections::HashSet;
use std::ffi::{CStr, CString};
use std::os::raw::*;
use std::rc::Rc;
use std::{fmt, mem, ptr, slice, str};

use gl::types::*;
use stdweb::unstable::TryInto;
use stdweb::web::html_element::CanvasElement;
use stdweb::web::{document, IParentNode};
use stdweb::{Reference, Value};
use unwind_aborts::unwind_aborts;
use webgl_stdweb as webgl;
use webgl_stdweb::{
    GLContext, WebGL2RenderingContext, WebGLActiveInfo, WebGLBuffer, WebGLFramebuffer,
    WebGLProgram, WebGLQuery, WebGLRenderbuffer, WebGLRenderingContext, WebGLSampler, WebGLShader,
    WebGLSync, WebGLTexture, WebGLTransformFeedback, WebGLUniformLocation, WebGLVertexArrayObject,
};

/// The OpenGL APIs supported by this library.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Api {
    /// OpenGL ES
    Gles,
}

/// Builder-style constructor for `Context`s.
#[derive(Debug, Clone)]
pub struct ContextBuilder {
    api: Api,
    version: (u8, u8),
    canvas: Option<CanvasSelector>,
}

impl ContextBuilder {
    /// Creates a `ContextBuilder` with the default values:
    ///
    /// - `gl_version` - OpenGL ES 2.0
    pub fn new() -> Self {
        Default::default()
    }

    /// Requests for a specific version of OpenGL, given the API and major/minor version numbers.
    ///
    /// Supported versions:
    ///
    /// - `Api::Gles, (2, 0)` (OpenGL ES 2.0)
    /// - `Api::Gles, (3, 0)` (OpenGL ES 3.0)
    ///
    /// Default: OpenGL ES 2.0
    pub fn gl_version(&mut self, api: Api, version: (u8, u8)) -> &mut Self {
        self.api = api;
        self.version = version;
        self
    }

    /// Fetches the HTML canvas element with the given ID and sets it as the target canvas.
    pub fn canvas_id(&mut self, id: &str) -> &mut Self {
        self.canvas = Some(CanvasSelector::ById(id.to_owned()));
        self
    }

    /// Sets the target canvas to the given `CanvasElement`.
    pub fn canvas(&mut self, canvas: &CanvasElement) -> &mut Self {
        self.canvas = Some(CanvasSelector::Explicit(canvas.clone()));
        self
    }

    /// Attempts to build a `Context` from this configuration.
    ///
    /// # Errors
    ///
    /// Context creation can fail if:
    ///
    /// - No canvas was specified or resolution of the canvas ID failed.
    /// - The required OpenGL version is not supported by this library.
    /// - The canvas failed to provide the required WebGL context.
    pub fn build(&self) -> Result<Context, Error> {
        let canvas = self
            .canvas
            .as_ref()
            .ok_or_else(|| Error::new("no canvas element was specified"))?
            .get_canvas()?;
        let webgl = match (self.api, self.version) {
            (Api::Gles, (2, 0)) => canvas
                .get_context::<WebGLRenderingContext>()
                .map_err(|e| Error::with_source(e, "unable to create WebGL context"))?
                .as_ref()
                .try_into()
                .unwrap(),
            (Api::Gles, (3, 0)) => canvas
                .get_context::<WebGL2RenderingContext>()
                .map_err(|e| Error::with_source(e, "unable to create WebGL context"))?
                .as_ref()
                .try_into()
                .unwrap(),
            other => {
                return Err(Error::new(format!(
                    "unsupported OpenGL version: {:?}",
                    other,
                )))
            }
        };
        Ok(Context::new(webgl))
    }
}

impl Default for ContextBuilder {
    fn default() -> Self {
        Self {
            api: Api::Gles,
            version: (2, 0),
            canvas: None,
        }
    }
}

/// Represents the ways a canvas can be identified by the user.
#[derive(Debug, Clone)]
enum CanvasSelector {
    /// The user provided the given string as the element ID.
    ById(String),

    /// The user provided a reference to the element directly.
    Explicit(CanvasElement),
}

impl CanvasSelector {
    /// Resolves the selector, producing a reference to the canvas element upon success.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    ///
    /// - The given ID was not valid.
    /// - There is no element with the given ID.
    /// - The element pointed to by the given ID is not a canvas element.
    fn get_canvas(&self) -> Result<Cow<CanvasElement>, Error> {
        match self {
            Self::ById(id) => {
                let canvas: CanvasElement = document()
                    .query_selector(id)
                    .map_err(|e| Error::with_source(e, "invalid id"))?
                    .ok_or_else(|| Error::new("no element with exists with the given id"))?
                    .try_into()
                    .map_err(|e| {
                        Error::with_source(e, "the given element is not a canvas element")
                    })?;
                Ok(Cow::Owned(canvas))
            }
            Self::Explicit(canvas) => Ok(Cow::Borrowed(&canvas)),
        }
    }
}

/// The generic error type returned by this crate in the case of failure.
pub struct Error {
    message: String,
    source: Option<Box<dyn std::error::Error + 'static>>,
}

impl Error {
    pub(crate) fn new<S>(message: S) -> Self
    where
        S: Into<String>,
    {
        Self {
            message: message.into(),
            source: None,
        }
    }

    pub(crate) fn with_source<E, S>(source: E, message: S) -> Self
    where
        E: std::error::Error + 'static,
        S: Into<String>,
    {
        Self {
            message: message.into(),
            source: Some(Box::new(source)),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.as_ref().map(AsRef::as_ref)
    }
}

/// A shared handle to an OpenGL context.
///
/// To construct `Context` instances, see `ContextBuilder`.
#[derive(Clone)]
pub struct Context {
    inner: Rc<RefCell<ContextInner>>,
}

impl Context {
    /// Creates a new context from a given WebGL context.
    pub(crate) fn new(webgl: GLContext) -> Self {
        Self {
            inner: Rc::new(RefCell::new(ContextInner {
                webgl,
                error_code: gl::NO_ERROR,
                shaders: ObjectMap::new(),
                buffers: ObjectMap::new(),
                framebuffers: ObjectMap::new(),
                queries: ObjectMap::new(),
                renderbuffers: ObjectMap::new(),
                samplers: ObjectMap::new(),
                syncs: ObjectMap::new(),
                textures: ObjectMap::new(),
                transform_feedbacks: ObjectMap::new(),
                vertex_arrays: ObjectMap::new(),
                uniforms: UniformMap::new(),
            })),
        }
    }

    /// Returns `true` if this context is currently active.
    pub fn is_current(&self) -> bool {
        is_current(Some(&self))
    }

    /// Makes this the currently active context.
    pub fn make_current(&self) {
        make_current(Some(self.clone()));
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Context")
            .field(&format_args!("{:p}", self.inner))
            .finish()
    }
}

impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl Eq for Context {}

/// The inner structure that contains context state.
struct ContextInner {
    /// The wrapped WebGL context.
    webgl: GLContext,

    /// Local error code storage, for errors generated by this layer. If there is no error,
    /// this will be equal to `NO_ERROR`.
    error_code: GLenum,

    /// Maps integer keys to JS program and shader objects.
    shaders: ObjectMap<ProgramOrShader>,

    /// Maps integer keys to JS buffer objects.
    buffers: ObjectMap<WebGLBuffer>,

    /// Maps integer keys to JS framebuffer objects.
    framebuffers: ObjectMap<WebGLFramebuffer>,

    /// Maps integer keys to JS query objects.
    queries: ObjectMap<WebGLQuery>,

    /// Maps integer keys to JS renderbuffer objects.
    renderbuffers: ObjectMap<WebGLRenderbuffer>,

    /// Maps integer keys to JS sampler objects.
    samplers: ObjectMap<WebGLSampler>,

    /// Maps integer keys to JS sync objects.
    /// (Keys are currently manually converted between integers and GLsyncs)
    syncs: ObjectMap<WebGLSync>,

    /// Maps integer keys to JS texture objects.
    textures: ObjectMap<WebGLTexture>,

    /// Maps integer keys to JS transform feedback objects.
    transform_feedbacks: ObjectMap<WebGLTransformFeedback>,

    /// Maps integer keys to JS vertex array objects.
    vertex_arrays: ObjectMap<WebGLVertexArrayObject>,

    /// Maps integer keys to per-program JS uniform locations.
    uniforms: UniformMap,
}

/// A mapping from integer keys to arbitrary object types. Used to maintain and query a mapping
/// between the integer object names used by OpenGL and the JS objects used by WebGL.
struct ObjectMap<T> {
    objects: Vec<Option<T>>,
}

impl<T> ObjectMap<T> {
    /// Constructs a new map with no registered objects.
    fn new() -> Self {
        Self {
            objects: vec![None],
        }
    }

    /// Gets the object corresponding to the given ID if one exists.
    ///
    /// # Errors
    ///
    /// Returns `INVALID_VALUE` if `id` is zero or does not have a corresponding object.
    fn get(&self, id: GLuint) -> Result<&T, GLenum> {
        self.objects
            .get(id as usize)
            .map(Option::as_ref)
            .flatten()
            .ok_or(gl::INVALID_VALUE)
    }

    /// Gets the object corresponding to the given ID if one exists, or `None` if the ID is zero.
    ///
    /// # Errors
    ///
    /// Returns `INVALID_VALUE` if `id` is not zero and does not have a corresponding object.
    fn get_nullable(&self, id: GLuint) -> Result<Option<&T>, GLenum> {
        if id == 0 {
            Ok(None)
        } else {
            self.get(id).map(Some)
        }
    }

    /// Gets the ID corresponding to the given object. If the object does not yet exist in the map,
    /// it will be assigned an ID.
    fn find(&mut self, obj: T) -> GLuint
    where
        T: AsRef<Reference>,
    {
        for (id, slot) in self.objects.iter_mut().enumerate().skip(1) {
            if slot.as_ref().map(AsRef::as_ref) == Some(obj.as_ref()) {
                return id as GLuint;
            }
        }
        self.add(Some(obj))
    }

    /// Adds the given object to the map, assigning it an ID, or returns zero if the input is
    /// `None`.
    fn add(&mut self, obj: Option<T>) -> GLuint {
        if obj.is_some() {
            for (id, slot) in self.objects.iter_mut().enumerate().skip(1) {
                if slot.is_none() {
                    *slot = obj;
                    return id as GLuint;
                }
            }
            let id = self.objects.len();
            self.objects.push(obj);
            id as GLuint
        } else {
            0
        }
    }

    /// Removes the entry corresponding to the given ID from the map, returning the corresponding
    /// object. If the ID is zero, `None` will be returned instead.
    ///
    /// # Errors
    ///
    /// Returns `INVALID_VALUE` if the ID is not zero and there is no entry corresponding to the
    /// given ID.
    fn remove(&mut self, id: GLuint) -> Result<Option<T>, GLenum> {
        if id == 0 {
            Ok(None)
        } else {
            self.objects
                .get_mut(id as usize)
                .map(Option::take)
                .flatten()
                .ok_or(gl::INVALID_VALUE)
                .map(Some)
        }
    }
}

/// Maintains a mapping from OpenGL's integer uniform locations to WebGL's JS objects.
/// The namespace of the integer IDs is per-program, so this map also accounts for which program is
/// currently used and being queried.
struct UniformMap {
    /// The most recent program to be passed to `UseProgram`, which is used as the implicit program
    /// ID in some queries.
    active_program: GLuint,

    /// The mapping from program IDs to uniform IDs to uniform objects.
    map: Vec<Vec<WebGLUniformLocation>>,
}

impl UniformMap {
    /// Constructs a new map with no entries.
    fn new() -> Self {
        Self {
            active_program: 0,
            map: vec![Vec::new()],
        }
    }

    /// References the sub-map of uniform locations for the given program ID.
    fn uniforms(&self, program: GLuint) -> &Vec<WebGLUniformLocation> {
        &self.map[program as usize]
    }

    /// Mutably references the sub-map of uniform locations for the given program ID.
    fn uniforms_mut(&mut self, program: GLuint) -> &mut Vec<WebGLUniformLocation> {
        &mut self.map[program as usize]
    }

    /// Gets the object corresponding to the given program and uniform location.
    ///
    /// # Errors
    ///
    /// Returns `INVALID_OPERATION` if the uniform location is -1 or the given program and uniform
    /// location does not have a corresponding object.
    fn get_obj(&self, program: GLuint, id: GLint) -> Result<&WebGLUniformLocation, GLenum> {
        self.get_obj_nullable(program, id)
            .and_then(|opt| opt.ok_or(gl::INVALID_OPERATION))
    }

    /// Gets the object corresponding to the given program and uniform location, or `None` if the
    /// location is `-1`.
    ///
    /// # Errors
    ///
    /// Returns `INVALID_OPERATION` if the uniform location is not -1 and the given program and
    /// uniform location does not have a corresponding object.
    fn get_obj_nullable(
        &self,
        program: GLuint,
        id: GLint,
    ) -> Result<Option<&WebGLUniformLocation>, GLenum> {
        if id < 0 {
            Ok(None)
        } else {
            self.uniforms(program)
                .get(id as usize)
                .ok_or(gl::INVALID_OPERATION)
                .map(Some)
        }
    }

    /// Equivalent to `get_obj_nullable`, with `program` being `self.active_program`.
    fn get_active_obj_nullable(&self, id: GLint) -> Result<Option<&WebGLUniformLocation>, GLenum> {
        self.get_obj_nullable(self.active_program, id)
    }

    /// Gets the uniform location ID corresponding to the given object for the active program, or
    /// `-1` if the input is `None`. If the object does not exist in the map, it will be assigned
    /// an ID.
    fn get_id(&mut self, obj: Option<WebGLUniformLocation>) -> GLint {
        let uniforms = self.uniforms_mut(self.active_program);
        if let Some(obj) = obj {
            for (i, registered) in uniforms.iter().enumerate() {
                if registered.as_ref() == obj.as_ref() {
                    return i as GLint;
                }
            }
            let id = uniforms.len();
            uniforms.push(obj);
            id as GLint
        } else {
            -1
        }
    }

    /// Sets the active program to the given program ID.
    fn using_program(&mut self, program_id: GLuint) {
        self.active_program = program_id;
    }

    /// (Re-)initializes the map for the given program ID.
    fn program_created(&mut self, program_id: GLuint) {
        if let Some(uniforms) = self.map.get_mut(program_id as usize) {
            // If this ID is reused, we don't want to keep the locations from the previous program.
            uniforms.clear();
        } else {
            // Otherwise, add the program to the map.
            self.map.resize(program_id as usize - 1, Vec::new())
        }
    }
}

/// Represents either a program or a shader object.
///
/// This is used to unify the integer namespace of program and shader objects, per the
/// specification [ref], by storing this object in a single map instead of having two separate
/// maps.
enum ProgramOrShader {
    Program(WebGLProgram),
    Shader(WebGLShader),
}

impl AsRef<Reference> for ProgramOrShader {
    fn as_ref(&self) -> &Reference {
        match self {
            Self::Program(program) => program.as_ref(),
            Self::Shader(shader) => shader.as_ref(),
        }
    }
}

impl ProgramOrShader {
    fn as_program(&self) -> Result<&WebGLProgram, GLenum> {
        match self {
            Self::Program(program) => Ok(program),
            _ => Err(gl::INVALID_OPERATION),
        }
    }

    fn as_shader(&self) -> Result<&WebGLShader, GLenum> {
        match self {
            Self::Shader(shader) => Ok(shader),
            _ => Err(gl::INVALID_OPERATION),
        }
    }

    fn to_program(self) -> Result<WebGLProgram, GLenum> {
        match self {
            Self::Program(program) => Ok(program),
            _ => Err(gl::INVALID_OPERATION),
        }
    }

    fn to_shader(self) -> Result<WebGLShader, GLenum> {
        match self {
            Self::Shader(shader) => Ok(shader),
            _ => Err(gl::INVALID_OPERATION),
        }
    }
}

thread_local! {
    static CURRENT_CONTEXT: RefCell<Option<Context>> = RefCell::new(None);
}

/// Returns whether the given context handle points to the current context.
///
/// If `context` is `None`, returns `true` if no context is active.
pub fn is_current(context: Option<&Context>) -> bool {
    CURRENT_CONTEXT.with(|cell| cell.borrow().as_ref() == context)
}

/// Makes the given context current, returning the previously current context if any.
///
/// If `context` is `None`, the current context is unset.
pub fn make_current(context: Option<Context>) -> Option<Context> {
    CURRENT_CONTEXT.with(|cell| mem::replace(&mut *cell.borrow_mut(), context))
}

fn with_context<F, R>(func: F) -> R
where
    F: FnOnce(&mut ContextInner) -> R,
{
    CURRENT_CONTEXT.with(|cell| {
        let cx = Ref::map(cell.borrow(), |opt| {
            opt.as_ref().expect("no context is currently active")
        });
        let mut cx_inner = cx.inner.borrow_mut();
        func(&mut cx_inner)
    })
}

fn try_with_context<F, R>(err_val: R, func: F) -> R
where
    F: FnOnce(&mut ContextInner) -> Result<R, GLenum>,
{
    with_context(|cx| match func(cx) {
        Ok(v) => v,
        Err(e) => {
            cx.error_code = e;
            err_val
        }
    })
}

fn user_bytes<'a>(size: GLsizei, data: *const c_void) -> &'a [u8] {
    user_array(size, data as *const _)
}

fn buffer_value<'a, T>(buffer: GLenum, values: *const T) -> &'a [T] {
    let size = match buffer {
        gl::COLOR => 4,
        gl::DEPTH => 1,
        gl::STENCIL => 1,
        _ => 0,
    };
    user_array(size, values)
}

fn user_array<'a, T>(size: GLsizei, data: *const T) -> &'a [T] {
    unsafe { slice::from_raw_parts(data, size as usize) }
}

fn user_array_nullable<'a, T>(size: GLsizei, data: *const T) -> Option<&'a [T]> {
    if data.is_null() {
        None
    } else {
        unsafe { Some(slice::from_raw_parts(data, size as usize)) }
    }
}

fn user_array_mut<'a, T>(size: GLsizei, data: *mut T) -> &'a mut [T] {
    unsafe { slice::from_raw_parts_mut(data, size as usize) }
}

fn webgl_boolean(v: GLboolean) -> webgl::GLboolean {
    v != 0
}

fn gl_boolean(v: webgl::GLboolean) -> GLboolean {
    v.into()
}

fn user_str<'a>(data: *const GLchar) -> Result<&'a str, GLenum> {
    unsafe { CStr::from_ptr(data).to_str().map_err(|_| gl::INVALID_VALUE) }
}

fn user_str_bounded<'a>(len: GLsizei, data: *const GLchar) -> Result<&'a str, GLenum> {
    unsafe {
        str::from_utf8(slice::from_raw_parts(data as *const u8, len as usize))
            .map_err(|_| gl::INVALID_VALUE)
    }
}

fn user_mut<'a, T>(data: *mut T) -> &'a mut T {
    unsafe { &mut *data }
}

fn user_nullable_mut<'a, T>(data: *mut T) -> Option<&'a mut T> {
    unsafe { data.as_mut() }
}

fn unpack_str(dst_len: GLsizei, len_out: *mut GLsizei, dst: *mut GLchar, src: &str) {
    let len_out = user_nullable_mut(len_out);
    let dst = user_array_mut(dst_len, dst as *mut u8);
    let src = src.as_bytes();

    let copy_len = src.len().min(dst.len() - 1);
    dst[..copy_len].copy_from_slice(&src[..copy_len]);
    dst[copy_len] = b'\0';

    if let Some(len_out) = len_out {
        *len_out = copy_len as GLsizei;
    }
}

fn unpack_array<T: Copy>(dst_len: GLsizei, len_out: *mut GLsizei, dst: *mut T, src: &[T]) {
    let len_out = user_nullable_mut(len_out);
    let dst = user_array_mut(dst_len, dst);

    let copy_len = src.len().min(dst.len());
    dst[..copy_len].copy_from_slice(&src[..copy_len]);

    if let Some(len_out) = len_out {
        *len_out = copy_len as GLsizei;
    }
}
fn unpack_active_info(
    buf_size: GLsizei,
    length: *mut GLsizei,
    size: *mut GLint,
    type_: *mut GLenum,
    name: *mut GLchar,
    info_src: WebGLActiveInfo,
) -> () {
    let size = user_mut(size);
    let type_ = user_mut(type_);

    unpack_str(buf_size, length, name, &info_src.name());
    *size = info_src.size();
    *type_ = info_src.type_();
}

trait Parameter: Sized {
    fn from_webgl(cx: &mut ContextInner, value: Value) -> Self;

    fn from_webgl_normalized(cx: &mut ContextInner, value: Value) -> Self {
        Self::from_webgl(cx, value)
    }
}

macro_rules! impl_parameter_for_int {
    ($($ty:ty),*$(,)?) => {$(

        impl Parameter for $ty {
            fn from_webgl(cx: &mut ContextInner, value: Value) -> Self {
                match value {
                    Value::Bool(true) => 1,
                    Value::Number(number) => {
                        let number = f64::from(number);
                        if number > Self::max_value() as f64 {
                            Self::max_value()
                        } else if number < Self::min_value() as f64 {
                            Self::min_value()
                        } else {
                            number.round() as Self
                        }
                    }
                    Value::Reference(ref reference) => {
                        use stdweb::unstable::TryInto;

                        if let Ok(shader) = reference.try_into() {
                            cx.shaders.find(ProgramOrShader::Shader(shader)) as Self
                        } else if let Ok(program) = reference.try_into() {
                            cx.shaders.find(ProgramOrShader::Program(program)) as Self
                        } else if let Ok(buffer) = reference.try_into() {
                            cx.buffers.find(buffer) as Self
                        } else if let Ok(framebuffer) = reference.try_into() {
                            cx.framebuffers.find(framebuffer) as Self
                        } else if let Ok(renderbuffer) = reference.try_into() {
                            cx.renderbuffers.find(renderbuffer) as Self
                        } else if let Ok(sampler) = reference.try_into() {
                            cx.samplers.find(sampler) as Self
                        } else if let Ok(texture) = reference.try_into() {
                            cx.textures.find(texture) as Self
                        } else if let Ok(tf) = reference.try_into() {
                            cx.transform_feedbacks.find(tf) as Self
                        } else if let Ok(va) = reference.try_into() {
                            cx.vertex_arrays.find(va) as Self
                        } else {
                            panic!("unknown reference type")
                        }
                    }
                    _ => 0,
                }
            }

            fn from_webgl_normalized(cx: &mut ContextInner, value: Value) -> Self {
                match value {
                    Value::Number(number) => {
                        // Equations 2.3 and 2.4 from the GLES 3.0 spec:
                        (f64::from(number) * Self::max_value() as f64).round() as Self
                    }
                    other => Self::from_webgl(cx, other),
                }
            }
        }
    )*};
}

impl_parameter_for_int!(GLint, GLuint, GLint64);

impl Parameter for GLboolean {
    fn from_webgl(_cx: &mut ContextInner, value: Value) -> Self {
        match value {
            Value::Bool(true) => gl::TRUE,
            Value::Number(number) if number != 0 => gl::TRUE,
            _ => gl::FALSE,
        }
    }
}

impl Parameter for GLfloat {
    fn from_webgl(_cx: &mut ContextInner, value: Value) -> Self {
        match value {
            Value::Bool(true) => 1.0,
            Value::Number(number) => f64::from(number) as GLfloat,
            _ => 0.0,
        }
    }
}

fn unpack_parameter<T: Parameter>(cx: &mut ContextInner, pname: GLenum, dst: *mut T, src: Value) {
    unpack_parameter_bounded(cx, pname, GLsizei::max_value(), ptr::null_mut(), dst, src)
}

fn unpack_parameter_bounded<T: Parameter>(
    cx: &mut ContextInner,
    pname: GLenum,
    buf_size: GLsizei,
    dst_len: *mut GLsizei,
    dst: *mut T,
    src: Value,
) {
    let normalized = match pname {
        gl::DEPTH_RANGE | gl::BLEND_COLOR | gl::COLOR_CLEAR_VALUE | gl::DEPTH_CLEAR_VALUE => true,
        _ => false,
    };
    if let Some(arr) = src.as_array() {
        let src: Vec<Value> = arr.into();
        let copy_len = buf_size.min(arr.len() as GLsizei);
        let dst_len = user_nullable_mut(dst_len);
        let dst = user_array_mut(copy_len, dst);

        for (dst, src) in dst.iter_mut().zip(src) {
            *dst = if normalized {
                Parameter::from_webgl_normalized(cx, src)
            } else {
                Parameter::from_webgl(cx, src)
            };
        }
        if let Some(dst_len) = dst_len {
            *dst_len = copy_len;
        }
    } else {
        let dst = user_mut(dst);
        *dst = if normalized {
            Parameter::from_webgl_normalized(cx, src)
        } else {
            Parameter::from_webgl(cx, src)
        }
    }
}

thread_local! {
    static STRING_CACHE: RefCell<HashSet<CString>> = RefCell::new(HashSet::new());
}

fn cache_string(value: Value) -> *const GLubyte {
    if let Some(s) = value.as_str() {
        STRING_CACHE.with(|cell| {
            let mut cache = cell.borrow_mut();
            let c_string = CString::new(s).unwrap();
            if !cache.contains(&c_string) {
                cache.insert(c_string.clone());
            }
            cache.get(&c_string).unwrap().as_ptr() as *const _
        })
    } else {
        ptr::null()
    }
}

/// Returns a pointer to a function given its name, or a null pointer if the named function is not
/// available. Designed to be passed to the `gl` crate like
/// `gl::load_with(aureum::get_proc_address)`.
pub fn get_proc_address(name: &str) -> *const c_void {
    match name {
        "glActiveTexture" => active_texture as *const _,
        "glAttachShader" => attach_shader as *const _,
        "glBeginQuery" => begin_query as *const _,
        "glBeginTransformFeedback" => begin_transform_feedback as *const _,
        "glBindAttribLocation" => bind_attrib_location as *const _,
        "glBindBuffer" => bind_buffer as *const _,
        "glBindBufferBase" => bind_buffer_base as *const _,
        "glBindBufferRange" => bind_buffer_range as *const _,
        "glBindFramebuffer" => bind_framebuffer as *const _,
        "glBindRenderbuffer" => bind_renderbuffer as *const _,
        "glBindSampler" => bind_sampler as *const _,
        "glBindTexture" => bind_texture as *const _,
        "glBindTransformFeedback" => bind_transform_feedback as *const _,
        "glBindVertexArray" => bind_vertex_array as *const _,
        "glBlendColor" => blend_color as *const _,
        "glBlendEquation" => blend_equation as *const _,
        "glBlendEquationSeparate" => blend_equation_separate as *const _,
        "glBlendFunc" => blend_func as *const _,
        "glBlendFuncSeparate" => blend_func_separate as *const _,
        "glBlitFramebuffer" => blit_framebuffer as *const _,
        "glBufferData" => buffer_data as *const _,
        "glBufferSubData" => buffer_sub_data as *const _,
        "glCheckFramebufferStatus" => check_framebuffer_status as *const _,
        "glClear" => clear as *const _,
        "glClearBufferfi" => clear_bufferfi as *const _,
        "glClearBufferfv" => clear_bufferfv as *const _,
        "glClearBufferiv" => clear_bufferiv as *const _,
        "glClearBufferuiv" => clear_bufferuiv as *const _,
        "glClearColor" => clear_color as *const _,
        "glClearDepthf" => clear_depthf as *const _,
        "glClearStencil" => clear_stencil as *const _,
        "glClientWaitSync" => client_wait_sync as *const _,
        "glColorMask" => color_mask as *const _,
        "glCompileShader" => compile_shader as *const _,
        "glCompressedTexImage2D" => compressed_tex_image2_d as *const _,
        "glCompressedTexImage3D" => compressed_tex_image3_d as *const _,
        "glCompressedTexSubImage2D" => compressed_tex_sub_image2_d as *const _,
        "glCompressedTexSubImage3D" => compressed_tex_sub_image3_d as *const _,
        "glCopyBufferSubData" => copy_buffer_sub_data as *const _,
        "glCopyTexImage2D" => copy_tex_image2_d as *const _,
        "glCopyTexSubImage2D" => copy_tex_sub_image2_d as *const _,
        "glCopyTexSubImage3D" => copy_tex_sub_image3_d as *const _,
        "glCreateProgram" => create_program as *const _,
        "glCreateShader" => create_shader as *const _,
        "glCullFace" => cull_face as *const _,
        "glDeleteBuffers" => delete_buffers as *const _,
        "glDeleteFramebuffers" => delete_framebuffers as *const _,
        "glDeleteProgram" => delete_program as *const _,
        "glDeleteQueries" => delete_queries as *const _,
        "glDeleteRenderbuffers" => delete_renderbuffers as *const _,
        "glDeleteSamplers" => delete_samplers as *const _,
        "glDeleteShader" => delete_shader as *const _,
        "glDeleteSync" => delete_sync as *const _,
        "glDeleteTextures" => delete_textures as *const _,
        "glDeleteTransformFeedbacks" => delete_transform_feedbacks as *const _,
        "glDeleteVertexArrays" => delete_vertex_arrays as *const _,
        "glDepthFunc" => depth_func as *const _,
        "glDepthMask" => depth_mask as *const _,
        "glDepthRangef" => depth_rangef as *const _,
        "glDetachShader" => detach_shader as *const _,
        "glDisable" => disable as *const _,
        "glDisableVertexAttribArray" => disable_vertex_attrib_array as *const _,
        "glDrawArrays" => draw_arrays as *const _,
        "glDrawArraysInstanced" => draw_arrays_instanced as *const _,
        "glDrawBuffers" => draw_buffers as *const _,
        "glDrawElements" => draw_elements as *const _,
        "glDrawElementsInstanced" => draw_elements_instanced as *const _,
        "glDrawRangeElements" => draw_range_elements as *const _,
        "glEnable" => enable as *const _,
        "glEnableVertexAttribArray" => enable_vertex_attrib_array as *const _,
        "glEndQuery" => end_query as *const _,
        "glEndTransformFeedback" => end_transform_feedback as *const _,
        "glFenceSync" => fence_sync as *const _,
        "glFinish" => finish as *const _,
        "glFlush" => flush as *const _,
        "glFlushMappedBufferRange" => flush_mapped_buffer_range as *const _,
        "glFramebufferRenderbuffer" => framebuffer_renderbuffer as *const _,
        "glFramebufferTexture2D" => framebuffer_texture2_d as *const _,
        "glFramebufferTextureLayer" => framebuffer_texture_layer as *const _,
        "glFrontFace" => front_face as *const _,
        "glGenBuffers" => gen_buffers as *const _,
        "glGenFramebuffers" => gen_framebuffers as *const _,
        "glGenQueries" => gen_queries as *const _,
        "glGenRenderbuffers" => gen_renderbuffers as *const _,
        "glGenSamplers" => gen_samplers as *const _,
        "glGenTextures" => gen_textures as *const _,
        "glGenTransformFeedbacks" => gen_transform_feedbacks as *const _,
        "glGenVertexArrays" => gen_vertex_arrays as *const _,
        "glGenerateMipmap" => generate_mipmap as *const _,
        "glGetActiveAttrib" => get_active_attrib as *const _,
        "glGetActiveUniform" => get_active_uniform as *const _,
        "glGetActiveUniformBlockName" => get_active_uniform_block_name as *const _,
        "glGetActiveUniformBlockiv" => get_active_uniform_blockiv as *const _,
        "glGetActiveUniformsiv" => get_active_uniformsiv as *const _,
        "glGetAttachedShaders" => get_attached_shaders as *const _,
        "glGetAttribLocation" => get_attrib_location as *const _,
        "glGetBooleanv" => get_booleanv as *const _,
        "glGetBufferParameteri64v" => get_buffer_parameteri64v as *const _,
        "glGetBufferParameteriv" => get_buffer_parameteriv as *const _,
        "glGetBufferPointerv" => get_buffer_pointerv as *const _,
        "glGetError" => get_error as *const _,
        "glGetFloatv" => get_floatv as *const _,
        "glGetFragDataLocation" => get_frag_data_location as *const _,
        "glGetFramebufferAttachmentParameteriv" => {
            get_framebuffer_attachment_parameteriv as *const _
        }
        "glGetInteger64i_v" => get_integer64i_v as *const _,
        "glGetInteger64v" => get_integer64v as *const _,
        "glGetIntegeri_v" => get_integeri_v as *const _,
        "glGetIntegerv" => get_integerv as *const _,
        "glGetInternalformativ" => get_internalformativ as *const _,
        "glGetProgramBinary" => get_program_binary as *const _,
        "glGetProgramInfoLog" => get_program_info_log as *const _,
        "glGetProgramiv" => get_programiv as *const _,
        "glGetQueryObjectuiv" => get_query_objectuiv as *const _,
        "glGetQueryiv" => get_queryiv as *const _,
        "glGetRenderbufferParameteriv" => get_renderbuffer_parameteriv as *const _,
        "glGetSamplerParameterfv" => get_sampler_parameterfv as *const _,
        "glGetSamplerParameteriv" => get_sampler_parameteriv as *const _,
        "glGetShaderInfoLog" => get_shader_info_log as *const _,
        "glGetShaderPrecisionFormat" => get_shader_precision_format as *const _,
        "glGetShaderSource" => get_shader_source as *const _,
        "glGetShaderiv" => get_shaderiv as *const _,
        "glGetString" => get_string as *const _,
        "glGetStringi" => get_stringi as *const _,
        "glGetSynciv" => get_synciv as *const _,
        "glGetTexParameterfv" => get_tex_parameterfv as *const _,
        "glGetTexParameteriv" => get_tex_parameteriv as *const _,
        "glGetTransformFeedbackVarying" => get_transform_feedback_varying as *const _,
        "glGetUniformBlockIndex" => get_uniform_block_index as *const _,
        "glGetUniformIndices" => get_uniform_indices as *const _,
        "glGetUniformLocation" => get_uniform_location as *const _,
        "glGetUniformfv" => get_uniformfv as *const _,
        "glGetUniformiv" => get_uniformiv as *const _,
        "glGetUniformuiv" => get_uniformuiv as *const _,
        "glGetVertexAttribIiv" => get_vertex_attrib_iiv as *const _,
        "glGetVertexAttribIuiv" => get_vertex_attrib_iuiv as *const _,
        "glGetVertexAttribPointerv" => get_vertex_attrib_pointerv as *const _,
        "glGetVertexAttribfv" => get_vertex_attribfv as *const _,
        "glGetVertexAttribiv" => get_vertex_attribiv as *const _,
        "glHint" => hint as *const _,
        "glInvalidateFramebuffer" => invalidate_framebuffer as *const _,
        "glInvalidateSubFramebuffer" => invalidate_sub_framebuffer as *const _,
        "glIsBuffer" => is_buffer as *const _,
        "glIsEnabled" => is_enabled as *const _,
        "glIsFramebuffer" => is_framebuffer as *const _,
        "glIsProgram" => is_program as *const _,
        "glIsQuery" => is_query as *const _,
        "glIsRenderbuffer" => is_renderbuffer as *const _,
        "glIsSampler" => is_sampler as *const _,
        "glIsShader" => is_shader as *const _,
        "glIsSync" => is_sync as *const _,
        "glIsTexture" => is_texture as *const _,
        "glIsTransformFeedback" => is_transform_feedback as *const _,
        "glIsVertexArray" => is_vertex_array as *const _,
        "glLineWidth" => line_width as *const _,
        "glLinkProgram" => link_program as *const _,
        "glMapBufferRange" => map_buffer_range as *const _,
        "glPauseTransformFeedback" => pause_transform_feedback as *const _,
        "glPixelStorei" => pixel_storei as *const _,
        "glPolygonOffset" => polygon_offset as *const _,
        "glProgramBinary" => program_binary as *const _,
        "glProgramParameteri" => program_parameteri as *const _,
        "glReadBuffer" => read_buffer as *const _,
        "glReadPixels" => read_pixels as *const _,
        "glReleaseShaderCompiler" => release_shader_compiler as *const _,
        "glRenderbufferStorage" => renderbuffer_storage as *const _,
        "glRenderbufferStorageMultisample" => renderbuffer_storage_multisample as *const _,
        "glResumeTransformFeedback" => resume_transform_feedback as *const _,
        "glSampleCoverage" => sample_coverage as *const _,
        "glSamplerParameterf" => sampler_parameterf as *const _,
        "glSamplerParameterfv" => sampler_parameterfv as *const _,
        "glSamplerParameteri" => sampler_parameteri as *const _,
        "glSamplerParameteriv" => sampler_parameteriv as *const _,
        "glScissor" => scissor as *const _,
        "glShaderBinary" => shader_binary as *const _,
        "glShaderSource" => shader_source as *const _,
        "glStencilFunc" => stencil_func as *const _,
        "glStencilFuncSeparate" => stencil_func_separate as *const _,
        "glStencilMask" => stencil_mask as *const _,
        "glStencilMaskSeparate" => stencil_mask_separate as *const _,
        "glStencilOp" => stencil_op as *const _,
        "glStencilOpSeparate" => stencil_op_separate as *const _,
        "glTexImage2D" => tex_image2_d as *const _,
        "glTexImage3D" => tex_image3_d as *const _,
        "glTexParameterf" => tex_parameterf as *const _,
        "glTexParameterfv" => tex_parameterfv as *const _,
        "glTexParameteri" => tex_parameteri as *const _,
        "glTexParameteriv" => tex_parameteriv as *const _,
        "glTexStorage2D" => tex_storage2_d as *const _,
        "glTexStorage3D" => tex_storage3_d as *const _,
        "glTexSubImage2D" => tex_sub_image2_d as *const _,
        "glTexSubImage3D" => tex_sub_image3_d as *const _,
        "glTransformFeedbackVaryings" => transform_feedback_varyings as *const _,
        "glUniform1f" => uniform1f as *const _,
        "glUniform1fv" => uniform1fv as *const _,
        "glUniform1i" => uniform1i as *const _,
        "glUniform1iv" => uniform1iv as *const _,
        "glUniform1ui" => uniform1ui as *const _,
        "glUniform1uiv" => uniform1uiv as *const _,
        "glUniform2f" => uniform2f as *const _,
        "glUniform2fv" => uniform2fv as *const _,
        "glUniform2i" => uniform2i as *const _,
        "glUniform2iv" => uniform2iv as *const _,
        "glUniform2ui" => uniform2ui as *const _,
        "glUniform2uiv" => uniform2uiv as *const _,
        "glUniform3f" => uniform3f as *const _,
        "glUniform3fv" => uniform3fv as *const _,
        "glUniform3i" => uniform3i as *const _,
        "glUniform3iv" => uniform3iv as *const _,
        "glUniform3ui" => uniform3ui as *const _,
        "glUniform3uiv" => uniform3uiv as *const _,
        "glUniform4f" => uniform4f as *const _,
        "glUniform4fv" => uniform4fv as *const _,
        "glUniform4i" => uniform4i as *const _,
        "glUniform4iv" => uniform4iv as *const _,
        "glUniform4ui" => uniform4ui as *const _,
        "glUniform4uiv" => uniform4uiv as *const _,
        "glUniformBlockBinding" => uniform_block_binding as *const _,
        "glUniformMatrix2fv" => uniform_matrix2fv as *const _,
        "glUniformMatrix2x3fv" => uniform_matrix2x3fv as *const _,
        "glUniformMatrix2x4fv" => uniform_matrix2x4fv as *const _,
        "glUniformMatrix3fv" => uniform_matrix3fv as *const _,
        "glUniformMatrix3x2fv" => uniform_matrix3x2fv as *const _,
        "glUniformMatrix3x4fv" => uniform_matrix3x4fv as *const _,
        "glUniformMatrix4fv" => uniform_matrix4fv as *const _,
        "glUniformMatrix4x2fv" => uniform_matrix4x2fv as *const _,
        "glUniformMatrix4x3fv" => uniform_matrix4x3fv as *const _,
        "glUnmapBuffer" => unmap_buffer as *const _,
        "glUseProgram" => use_program as *const _,
        "glValidateProgram" => validate_program as *const _,
        "glVertexAttrib1f" => vertex_attrib1f as *const _,
        "glVertexAttrib1fv" => vertex_attrib1fv as *const _,
        "glVertexAttrib2f" => vertex_attrib2f as *const _,
        "glVertexAttrib2fv" => vertex_attrib2fv as *const _,
        "glVertexAttrib3f" => vertex_attrib3f as *const _,
        "glVertexAttrib3fv" => vertex_attrib3fv as *const _,
        "glVertexAttrib4f" => vertex_attrib4f as *const _,
        "glVertexAttrib4fv" => vertex_attrib4fv as *const _,
        "glVertexAttribDivisor" => vertex_attrib_divisor as *const _,
        "glVertexAttribI4i" => vertex_attrib_i4i as *const _,
        "glVertexAttribI4iv" => vertex_attrib_i4iv as *const _,
        "glVertexAttribI4ui" => vertex_attrib_i4ui as *const _,
        "glVertexAttribI4uiv" => vertex_attrib_i4uiv as *const _,
        "glVertexAttribIPointer" => vertex_attrib_i_pointer as *const _,
        "glVertexAttribPointer" => vertex_attrib_pointer as *const _,
        "glViewport" => viewport as *const _,
        "glWaitSync" => wait_sync as *const _,
        _ => ptr::null(),
    }
}

#[unwind_aborts]
extern "system" fn active_texture(texture: GLenum) -> () {
    with_context(|cx| cx.webgl.active_texture(texture))
}

#[unwind_aborts]
extern "system" fn attach_shader(program: GLuint, shader: GLuint) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let shader = cx.shaders.get(shader)?.as_shader()?;
        Ok(cx.webgl.attach_shader(program, shader))
    })
}

#[unwind_aborts]
extern "system" fn begin_query(target: GLenum, id: GLuint) -> () {
    try_with_context((), |cx| {
        let query = cx.queries.get(id)?;
        Ok(cx.webgl.begin_query(target, query))
    })
}

#[unwind_aborts]
extern "system" fn begin_transform_feedback(primitive_mode: GLenum) -> () {
    with_context(|cx| cx.webgl.begin_transform_feedback(primitive_mode))
}

#[unwind_aborts]
extern "system" fn bind_attrib_location(program: GLuint, index: GLuint, name: *const GLchar) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let name = user_str(name)?;
        Ok(cx.webgl.bind_attrib_location(program, index, name))
    })
}

#[unwind_aborts]
extern "system" fn bind_buffer(target: GLenum, buffer: GLuint) -> () {
    try_with_context((), |cx| {
        let buffer = cx.buffers.get_nullable(buffer)?;
        Ok(cx.webgl.bind_buffer(target, buffer))
    })
}

#[unwind_aborts]
extern "system" fn bind_buffer_base(target: GLenum, index: GLuint, buffer: GLuint) -> () {
    try_with_context((), |cx| {
        let buffer = cx.buffers.get_nullable(buffer)?;
        Ok(cx.webgl.bind_buffer_base(target, index, buffer))
    })
}

#[unwind_aborts]
extern "system" fn bind_buffer_range(
    target: GLenum,
    index: GLuint,
    buffer: GLuint,
    offset: GLintptr,
    size: GLsizeiptr,
) -> () {
    try_with_context((), |cx| {
        let buffer = cx.buffers.get_nullable(buffer)?;
        Ok(cx.webgl.bind_buffer_range(
            target,
            index,
            buffer,
            offset as webgl::GLintptr,
            size as webgl::GLintptr,
        ))
    })
}

#[unwind_aborts]
extern "system" fn bind_framebuffer(target: GLenum, framebuffer: GLuint) -> () {
    try_with_context((), |cx| {
        let framebuffer = cx.framebuffers.get_nullable(framebuffer)?;
        Ok(cx.webgl.bind_framebuffer(target, framebuffer))
    })
}

#[unwind_aborts]
extern "system" fn bind_renderbuffer(target: GLenum, renderbuffer: GLuint) -> () {
    try_with_context((), |cx| {
        let renderbuffer = cx.renderbuffers.get_nullable(renderbuffer)?;
        Ok(cx.webgl.bind_renderbuffer(target, renderbuffer))
    })
}

#[unwind_aborts]
extern "system" fn bind_sampler(unit: GLuint, sampler: GLuint) -> () {
    try_with_context((), |cx| {
        let sampler = cx.samplers.get_nullable(sampler)?;
        Ok(cx.webgl.bind_sampler(unit, sampler))
    })
}

#[unwind_aborts]
extern "system" fn bind_texture(target: GLenum, texture: GLuint) -> () {
    try_with_context((), |cx| {
        let texture = cx.textures.get_nullable(texture)?;
        Ok(cx.webgl.bind_texture(target, texture))
    })
}

#[unwind_aborts]
extern "system" fn bind_transform_feedback(target: GLenum, id: GLuint) -> () {
    try_with_context((), |cx| {
        let tf = cx.transform_feedbacks.get_nullable(id)?;
        Ok(cx.webgl.bind_transform_feedback(target, tf))
    })
}

#[unwind_aborts]
extern "system" fn bind_vertex_array(array: GLuint) -> () {
    try_with_context((), |cx| {
        let array = cx.vertex_arrays.get_nullable(array)?;
        Ok(cx.webgl.bind_vertex_array(array))
    })
}

#[unwind_aborts]
extern "system" fn blend_color(red: GLfloat, green: GLfloat, blue: GLfloat, alpha: GLfloat) -> () {
    with_context(|cx| cx.webgl.blend_color(red, green, blue, alpha))
}

#[unwind_aborts]
extern "system" fn blend_equation(mode: GLenum) -> () {
    with_context(|cx| cx.webgl.blend_equation(mode))
}

#[unwind_aborts]
extern "system" fn blend_equation_separate(mode_rgb: GLenum, mode_alpha: GLenum) -> () {
    with_context(|cx| cx.webgl.blend_equation_separate(mode_rgb, mode_alpha))
}

#[unwind_aborts]
extern "system" fn blend_func(sfactor: GLenum, dfactor: GLenum) -> () {
    with_context(|cx| cx.webgl.blend_func(sfactor, dfactor))
}

#[unwind_aborts]
extern "system" fn blend_func_separate(
    sfactor_rgb: GLenum,
    dfactor_rgb: GLenum,
    sfactor_alpha: GLenum,
    dfactor_alpha: GLenum,
) -> () {
    with_context(|cx| {
        cx.webgl
            .blend_func_separate(sfactor_rgb, dfactor_rgb, sfactor_alpha, dfactor_alpha)
    })
}

#[unwind_aborts]
extern "system" fn blit_framebuffer(
    src_x0: GLint,
    src_y0: GLint,
    src_x1: GLint,
    src_y1: GLint,
    dst_x0: GLint,
    dst_y0: GLint,
    dst_x1: GLint,
    dst_y1: GLint,
    mask: GLbitfield,
    filter: GLenum,
) -> () {
    with_context(|cx| {
        cx.webgl.blit_framebuffer(
            src_x0, src_y0, src_x1, src_y1, dst_x0, dst_y0, dst_x1, dst_y1, mask, filter,
        )
    })
}

#[unwind_aborts]
extern "system" fn buffer_data(
    target: GLenum,
    size: GLsizeiptr,
    data: *const c_void,
    usage: GLenum,
) -> () {
    let data = user_bytes(size as GLsizei, data);
    with_context(|cx| cx.webgl.buffer_data_2(target, data, usage, 0, 0))
}

#[unwind_aborts]
extern "system" fn buffer_sub_data(
    target: GLenum,
    offset: GLintptr,
    size: GLsizeiptr,
    data: *const c_void,
) -> () {
    let data = user_bytes(size as GLsizei, data);
    with_context(|cx| {
        cx.webgl
            .buffer_sub_data_1(target, offset as webgl::GLintptr, data, 0, 0)
    })
}

#[unwind_aborts]
extern "system" fn check_framebuffer_status(target: GLenum) -> GLenum {
    with_context(|cx| cx.webgl.check_framebuffer_status(target))
}

#[unwind_aborts]
extern "system" fn clear(mask: GLbitfield) -> () {
    with_context(|cx| cx.webgl.clear(mask))
}

#[unwind_aborts]
extern "system" fn clear_bufferfi(
    buffer: GLenum,
    drawbuffer: GLint,
    depth: GLfloat,
    stencil: GLint,
) -> () {
    with_context(|cx| cx.webgl.clear_bufferfi(buffer, drawbuffer, depth, stencil))
}

#[unwind_aborts]
extern "system" fn clear_bufferfv(buffer: GLenum, drawbuffer: GLint, value: *const GLfloat) -> () {
    let value = buffer_value(buffer, value);
    with_context(|cx| cx.webgl.clear_bufferfv(buffer, drawbuffer, value, 0))
}

#[unwind_aborts]
extern "system" fn clear_bufferiv(buffer: GLenum, drawbuffer: GLint, value: *const GLint) -> () {
    let value = buffer_value(buffer, value);
    with_context(|cx| cx.webgl.clear_bufferiv(buffer, drawbuffer, value, 0))
}

#[unwind_aborts]
extern "system" fn clear_bufferuiv(buffer: GLenum, drawbuffer: GLint, value: *const GLuint) -> () {
    let value = buffer_value(buffer, value);
    with_context(|cx| cx.webgl.clear_bufferuiv(buffer, drawbuffer, value, 0))
}

#[unwind_aborts]
extern "system" fn clear_color(red: GLfloat, green: GLfloat, blue: GLfloat, alpha: GLfloat) -> () {
    with_context(|cx| cx.webgl.clear_color(red, green, blue, alpha))
}

#[unwind_aborts]
extern "system" fn clear_depthf(d: GLfloat) -> () {
    with_context(|cx| cx.webgl.clear_depth(d))
}

#[unwind_aborts]
extern "system" fn clear_stencil(s: GLint) -> () {
    with_context(|cx| cx.webgl.clear_stencil(s))
}

#[unwind_aborts]
extern "system" fn client_wait_sync(sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> GLenum {
    try_with_context(gl::WAIT_FAILED, |cx| {
        let sync = cx.syncs.get(sync as GLuint)?;
        Ok(cx.webgl.client_wait_sync(sync, flags, timeout))
    })
}

#[unwind_aborts]
extern "system" fn color_mask(
    red: GLboolean,
    green: GLboolean,
    blue: GLboolean,
    alpha: GLboolean,
) -> () {
    let red = webgl_boolean(red);
    let green = webgl_boolean(green);
    let blue = webgl_boolean(blue);
    let alpha = webgl_boolean(alpha);
    with_context(|cx| cx.webgl.color_mask(red, green, blue, alpha))
}

#[unwind_aborts]
extern "system" fn compile_shader(shader: GLuint) -> () {
    try_with_context((), |cx| {
        let shader = cx.shaders.get(shader)?.as_shader()?;
        Ok(cx.webgl.compile_shader(shader))
    })
}

#[unwind_aborts]
extern "system" fn compressed_tex_image2_d(
    target: GLenum,
    level: GLint,
    internalformat: GLenum,
    width: GLsizei,
    height: GLsizei,
    border: GLint,
    image_size: GLsizei,
    data: *const c_void,
) -> () {
    let data = user_bytes(image_size, data);
    with_context(|cx| {
        cx.webgl.compressed_tex_image2_d_1(
            target,
            level,
            internalformat,
            width,
            height,
            border,
            data,
            0,
            0,
        )
    })
}

#[unwind_aborts]
extern "system" fn compressed_tex_image3_d(
    target: GLenum,
    level: GLint,
    internalformat: GLenum,
    width: GLsizei,
    height: GLsizei,
    depth: GLsizei,
    border: GLint,
    image_size: GLsizei,
    data: *const c_void,
) -> () {
    let data = user_bytes(image_size, data);
    with_context(|cx| {
        cx.webgl.compressed_tex_image3_d_1(
            target,
            level,
            internalformat,
            width,
            height,
            depth,
            border,
            data,
            0,
            0,
        )
    })
}

#[unwind_aborts]
extern "system" fn compressed_tex_sub_image2_d(
    target: GLenum,
    level: GLint,
    xoffset: GLint,
    yoffset: GLint,
    width: GLsizei,
    height: GLsizei,
    format: GLenum,
    image_size: GLsizei,
    data: *const c_void,
) -> () {
    let data = user_bytes(image_size, data);
    with_context(|cx| {
        cx.webgl.compressed_tex_sub_image2_d_1(
            target, level, xoffset, yoffset, width, height, format, data, 0, 0,
        )
    })
}

#[unwind_aborts]
extern "system" fn compressed_tex_sub_image3_d(
    target: GLenum,
    level: GLint,
    xoffset: GLint,
    yoffset: GLint,
    zoffset: GLint,
    width: GLsizei,
    height: GLsizei,
    depth: GLsizei,
    format: GLenum,
    image_size: GLsizei,
    data: *const c_void,
) -> () {
    let data = user_bytes(image_size, data);
    with_context(|cx| {
        cx.webgl.compressed_tex_sub_image3_d_1(
            target, level, xoffset, yoffset, zoffset, width, height, depth, format, data, 0, 0,
        )
    })
}

#[unwind_aborts]
extern "system" fn copy_buffer_sub_data(
    read_target: GLenum,
    write_target: GLenum,
    read_offset: GLintptr,
    write_offset: GLintptr,
    size: GLsizeiptr,
) -> () {
    with_context(|cx| {
        cx.webgl.copy_buffer_sub_data(
            read_target,
            write_target,
            read_offset as webgl::GLintptr,
            write_offset as webgl::GLintptr,
            size as webgl::GLintptr,
        )
    })
}

#[unwind_aborts]
extern "system" fn copy_tex_image2_d(
    target: GLenum,
    level: GLint,
    internalformat: GLenum,
    x: GLint,
    y: GLint,
    width: GLsizei,
    height: GLsizei,
    border: GLint,
) -> () {
    with_context(|cx| {
        cx.webgl
            .copy_tex_image2_d(target, level, internalformat, x, y, width, height, border)
    })
}

#[unwind_aborts]
extern "system" fn copy_tex_sub_image2_d(
    target: GLenum,
    level: GLint,
    xoffset: GLint,
    yoffset: GLint,
    x: GLint,
    y: GLint,
    width: GLsizei,
    height: GLsizei,
) -> () {
    with_context(|cx| {
        cx.webgl
            .copy_tex_sub_image2_d(target, level, xoffset, yoffset, x, y, width, height)
    })
}

#[unwind_aborts]
extern "system" fn copy_tex_sub_image3_d(
    target: GLenum,
    level: GLint,
    xoffset: GLint,
    yoffset: GLint,
    zoffset: GLint,
    x: GLint,
    y: GLint,
    width: GLsizei,
    height: GLsizei,
) -> () {
    with_context(|cx| {
        cx.webgl.copy_tex_sub_image3_d(
            target, level, xoffset, yoffset, zoffset, x, y, width, height,
        )
    })
}

#[unwind_aborts]
extern "system" fn create_program() -> GLuint {
    with_context(|cx| {
        let program = cx.webgl.create_program();
        let id = cx.shaders.add(program.map(ProgramOrShader::Program));
        cx.uniforms.program_created(id);
        id
    })
}

#[unwind_aborts]
extern "system" fn create_shader(type_: GLenum) -> GLuint {
    with_context(|cx| {
        let shader = cx.webgl.create_shader(type_);
        cx.shaders.add(shader.map(ProgramOrShader::Shader))
    })
}

#[unwind_aborts]
extern "system" fn cull_face(mode: GLenum) -> () {
    with_context(|cx| cx.webgl.cull_face(mode))
}

#[unwind_aborts]
extern "system" fn delete_buffers(n: GLsizei, buffers: *const GLuint) -> () {
    let buffers = user_array(n, buffers);
    with_context(|cx| {
        for &buffer in buffers {
            if let Ok(buffer) = cx.buffers.get_nullable(buffer) {
                cx.webgl.delete_buffer(buffer);
            }
        }
    })
}

#[unwind_aborts]
extern "system" fn delete_framebuffers(n: GLsizei, framebuffers: *const GLuint) -> () {
    let framebuffers = user_array(n, framebuffers);
    with_context(|cx| {
        for &fb in framebuffers {
            if let Ok(fb) = cx.framebuffers.get_nullable(fb) {
                cx.webgl.delete_framebuffer(fb);
            }
        }
    })
}

#[unwind_aborts]
extern "system" fn delete_program(program: GLuint) -> () {
    try_with_context((), |cx| {
        let program = cx
            .shaders
            .remove(program)?
            .map(ProgramOrShader::to_program)
            .transpose()?;
        Ok(cx.webgl.delete_program(program.as_ref()))
    })
}

#[unwind_aborts]
extern "system" fn delete_queries(n: GLsizei, ids: *const GLuint) -> () {
    let ids = user_array(n, ids);
    with_context(|cx| {
        for &id in ids {
            if let Ok(query) = cx.queries.get_nullable(id) {
                cx.webgl.delete_query(query);
            }
        }
    })
}

#[unwind_aborts]
extern "system" fn delete_renderbuffers(n: GLsizei, renderbuffers: *const GLuint) -> () {
    let renderbuffers = user_array(n, renderbuffers);
    with_context(|cx| {
        for &rb in renderbuffers {
            if let Ok(rb) = cx.renderbuffers.get_nullable(rb) {
                cx.webgl.delete_renderbuffer(rb);
            }
        }
    })
}

#[unwind_aborts]
extern "system" fn delete_samplers(count: GLsizei, samplers: *const GLuint) -> () {
    let samplers = user_array(count, samplers);
    with_context(|cx| {
        for &sampler in samplers {
            if let Ok(sampler) = cx.samplers.get_nullable(sampler) {
                cx.webgl.delete_sampler(sampler);
            }
        }
    })
}

#[unwind_aborts]
extern "system" fn delete_shader(shader: GLuint) -> () {
    try_with_context((), |cx| {
        let shader = cx
            .shaders
            .remove(shader)?
            .map(ProgramOrShader::to_shader)
            .transpose()?;
        Ok(cx.webgl.delete_shader(shader.as_ref()))
    })
}

#[unwind_aborts]
extern "system" fn delete_sync(sync: GLsync) -> () {
    try_with_context((), |cx| {
        let sync = cx.syncs.remove(sync as GLuint)?;
        Ok(cx.webgl.delete_sync(sync.as_ref()))
    })
}

#[unwind_aborts]
extern "system" fn delete_textures(n: GLsizei, textures: *const GLuint) -> () {
    let textures = user_array(n, textures);
    with_context(|cx| {
        for &texture in textures {
            if let Ok(texture) = cx.textures.get_nullable(texture) {
                cx.webgl.delete_texture(texture)
            }
        }
    })
}

#[unwind_aborts]
extern "system" fn delete_transform_feedbacks(n: GLsizei, ids: *const GLuint) -> () {
    let ids = user_array(n, ids);
    with_context(|cx| {
        for &id in ids {
            if let Ok(tf) = cx.transform_feedbacks.get_nullable(id) {
                cx.webgl.delete_transform_feedback(tf);
            }
        }
    })
}

#[unwind_aborts]
extern "system" fn delete_vertex_arrays(n: GLsizei, arrays: *const GLuint) -> () {
    let arrays = user_array(n, arrays);
    with_context(|cx| {
        for &va in arrays {
            if let Ok(va) = cx.vertex_arrays.get_nullable(va) {
                cx.webgl.delete_vertex_array(va);
            }
        }
    })
}

#[unwind_aborts]
extern "system" fn depth_func(func: GLenum) -> () {
    with_context(|cx| cx.webgl.depth_func(func))
}

#[unwind_aborts]
extern "system" fn depth_mask(flag: GLboolean) -> () {
    let flag = webgl_boolean(flag);
    with_context(|cx| cx.webgl.depth_mask(flag))
}

#[unwind_aborts]
extern "system" fn depth_rangef(n: GLfloat, f: GLfloat) -> () {
    with_context(|cx| cx.webgl.depth_range(n, f))
}

#[unwind_aborts]
extern "system" fn detach_shader(program: GLuint, shader: GLuint) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let shader = cx.shaders.get(shader)?.as_shader()?;
        Ok(cx.webgl.detach_shader(program, shader))
    })
}

#[unwind_aborts]
extern "system" fn disable(cap: GLenum) -> () {
    with_context(|cx| cx.webgl.disable(cap))
}

#[unwind_aborts]
extern "system" fn disable_vertex_attrib_array(index: GLuint) -> () {
    with_context(|cx| cx.webgl.disable_vertex_attrib_array(index))
}

#[unwind_aborts]
extern "system" fn draw_arrays(mode: GLenum, first: GLint, count: GLsizei) -> () {
    with_context(|cx| cx.webgl.draw_arrays(mode, first, count))
}

#[unwind_aborts]
extern "system" fn draw_arrays_instanced(
    mode: GLenum,
    first: GLint,
    count: GLsizei,
    instancecount: GLsizei,
) -> () {
    with_context(|cx| {
        cx.webgl
            .draw_arrays_instanced(mode, first, count, instancecount)
    })
}

#[unwind_aborts]
extern "system" fn draw_buffers(n: GLsizei, bufs: *const GLenum) -> () {
    let bufs = user_array(n, bufs);
    with_context(|cx| cx.webgl.draw_buffers(bufs))
}

#[unwind_aborts]
extern "system" fn draw_elements(
    mode: GLenum,
    count: GLsizei,
    type_: GLenum,
    indices: *const c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::ELEMENT_ARRAY_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl
                .draw_elements(mode, count, type_, indices as webgl::GLintptr);
        }
    })
}

#[unwind_aborts]
extern "system" fn draw_elements_instanced(
    mode: GLenum,
    count: GLsizei,
    type_: GLenum,
    indices: *const c_void,
    instancecount: GLsizei,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::ELEMENT_ARRAY_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl.draw_elements_instanced(
                mode,
                count,
                type_,
                indices as webgl::GLintptr,
                instancecount,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn draw_range_elements(
    mode: GLenum,
    start: GLuint,
    end: GLuint,
    count: GLsizei,
    type_: GLenum,
    indices: *const c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::ELEMENT_ARRAY_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl.draw_range_elements(
                mode,
                start,
                end,
                count,
                type_,
                indices as webgl::GLintptr,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn enable(cap: GLenum) -> () {
    with_context(|cx| cx.webgl.enable(cap))
}

#[unwind_aborts]
extern "system" fn enable_vertex_attrib_array(index: GLuint) -> () {
    with_context(|cx| cx.webgl.enable_vertex_attrib_array(index))
}

#[unwind_aborts]
extern "system" fn end_query(target: GLenum) -> () {
    with_context(|cx| cx.webgl.end_query(target))
}

#[unwind_aborts]
extern "system" fn end_transform_feedback() -> () {
    with_context(|cx| cx.webgl.end_transform_feedback())
}

#[unwind_aborts]
extern "system" fn fence_sync(condition: GLenum, flags: GLbitfield) -> GLsync {
    with_context(|cx| cx.syncs.add(cx.webgl.fence_sync(condition, flags)) as GLsync)
}

#[unwind_aborts]
extern "system" fn finish() -> () {
    with_context(|cx| cx.webgl.finish())
}

#[unwind_aborts]
extern "system" fn flush() -> () {
    with_context(|cx| cx.webgl.flush())
}

#[unwind_aborts]
extern "system" fn flush_mapped_buffer_range(
    _target: GLenum,
    _offset: GLintptr,
    _length: GLsizeiptr,
) -> () {
    unimplemented!("client-side buffers are not fully supported");
}

#[unwind_aborts]
extern "system" fn framebuffer_renderbuffer(
    target: GLenum,
    attachment: GLenum,
    renderbuffertarget: GLenum,
    renderbuffer: GLuint,
) -> () {
    try_with_context((), |cx| {
        let rb = cx.renderbuffers.get_nullable(renderbuffer)?;
        Ok(cx
            .webgl
            .framebuffer_renderbuffer(target, attachment, renderbuffertarget, rb))
    })
}

#[unwind_aborts]
extern "system" fn framebuffer_texture2_d(
    target: GLenum,
    attachment: GLenum,
    textarget: GLenum,
    texture: GLuint,
    level: GLint,
) -> () {
    try_with_context((), |cx| {
        let texture = cx.textures.get_nullable(texture)?;
        Ok(cx
            .webgl
            .framebuffer_texture2_d(target, attachment, textarget, texture, level))
    })
}

#[unwind_aborts]
extern "system" fn framebuffer_texture_layer(
    target: GLenum,
    attachment: GLenum,
    texture: GLuint,
    level: GLint,
    layer: GLint,
) -> () {
    try_with_context((), |cx| {
        let texture = cx.textures.get_nullable(texture)?;
        Ok(cx
            .webgl
            .framebuffer_texture_layer(target, attachment, texture, level, layer))
    })
}

#[unwind_aborts]
extern "system" fn front_face(mode: GLenum) -> () {
    with_context(|cx| cx.webgl.front_face(mode))
}

#[unwind_aborts]
extern "system" fn gen_buffers(n: GLsizei, buffers: *mut GLuint) -> () {
    let buffers = user_array_mut(n, buffers);
    with_context(|cx| {
        for buffer in buffers {
            *buffer = cx.buffers.add(cx.webgl.create_buffer());
        }
    })
}

#[unwind_aborts]
extern "system" fn gen_framebuffers(n: GLsizei, framebuffers: *mut GLuint) -> () {
    let framebuffers = user_array_mut(n, framebuffers);
    with_context(|cx| {
        for fb in framebuffers {
            *fb = cx.framebuffers.add(cx.webgl.create_framebuffer());
        }
    })
}

#[unwind_aborts]
extern "system" fn gen_queries(n: GLsizei, ids: *mut GLuint) -> () {
    let ids = user_array_mut(n, ids);
    with_context(|cx| {
        for id in ids {
            *id = cx.queries.add(cx.webgl.create_query());
        }
    })
}

#[unwind_aborts]
extern "system" fn gen_renderbuffers(n: GLsizei, renderbuffers: *mut GLuint) -> () {
    let renderbuffers = user_array_mut(n, renderbuffers);
    with_context(|cx| {
        for rb in renderbuffers {
            *rb = cx.renderbuffers.add(cx.webgl.create_renderbuffer());
        }
    })
}

#[unwind_aborts]
extern "system" fn gen_samplers(count: GLsizei, samplers: *mut GLuint) -> () {
    let samplers = user_array_mut(count, samplers);
    with_context(|cx| {
        for sampler in samplers {
            *sampler = cx.samplers.add(cx.webgl.create_sampler());
        }
    })
}

#[unwind_aborts]
extern "system" fn gen_textures(n: GLsizei, textures: *mut GLuint) -> () {
    let textures = user_array_mut(n, textures);
    with_context(|cx| {
        for texture in textures {
            *texture = cx.textures.add(cx.webgl.create_texture());
        }
    })
}

#[unwind_aborts]
extern "system" fn gen_transform_feedbacks(n: GLsizei, ids: *mut GLuint) -> () {
    let ids = user_array_mut(n, ids);
    with_context(|cx| {
        for id in ids {
            *id = cx
                .transform_feedbacks
                .add(cx.webgl.create_transform_feedback());
        }
    })
}

#[unwind_aborts]
extern "system" fn gen_vertex_arrays(n: GLsizei, arrays: *mut GLuint) -> () {
    let arrays = user_array_mut(n, arrays);
    with_context(|cx| {
        for va in arrays {
            *va = cx.vertex_arrays.add(cx.webgl.create_vertex_array());
        }
    })
}

#[unwind_aborts]
extern "system" fn generate_mipmap(target: GLenum) -> () {
    with_context(|cx| cx.webgl.generate_mipmap(target))
}

#[unwind_aborts]
extern "system" fn get_active_attrib(
    program: GLuint,
    index: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    size: *mut GLint,
    type_: *mut GLenum,
    name: *mut GLchar,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        if let Some(info) = cx.webgl.get_active_attrib(program, index) {
            unpack_active_info(buf_size, length, size, type_, name, info);
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_active_uniform(
    program: GLuint,
    index: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    size: *mut GLint,
    type_: *mut GLenum,
    name: *mut GLchar,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        if let Some(info) = cx.webgl.get_active_uniform(program, index) {
            unpack_active_info(buf_size, length, size, type_, name, info);
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_active_uniform_block_name(
    program: GLuint,
    uniform_block_index: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    uniform_block_name: *mut GLchar,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        if let Some(name) = cx
            .webgl
            .get_active_uniform_block_name(program, uniform_block_index)
        {
            unpack_str(buf_size, length, uniform_block_name, &name)
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_active_uniform_blockiv(
    program: GLuint,
    uniform_block_index: GLuint,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let value =
            cx.webgl
                .get_active_uniform_block_parameter(program, uniform_block_index, pname);
        unpack_parameter(cx, pname, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_active_uniformsiv(
    program: GLuint,
    uniform_count: GLsizei,
    uniform_indices: *const GLuint,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let uniform_indices = user_array(uniform_count, uniform_indices);
        let value = cx
            .webgl
            .get_active_uniforms(program, uniform_indices, pname);
        unpack_parameter(cx, pname, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_attached_shaders(
    program: GLuint,
    max_count: GLsizei,
    count: *mut GLsizei,
    shaders: *mut GLuint,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        if let Some(shader_objs) = cx.webgl.get_attached_shaders(program) {
            let shader_ids: Vec<_> = shader_objs
                .into_iter()
                .map(|obj| cx.shaders.find(ProgramOrShader::Shader(obj)))
                .collect();
            unpack_array(max_count, count, shaders, &shader_ids);
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_attrib_location(program: GLuint, name: *const GLchar) -> GLint {
    try_with_context(-1, |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let name = user_str(name)?;
        Ok(cx.webgl.get_attrib_location(program, name))
    })
}

#[unwind_aborts]
extern "system" fn get_booleanv(pname: GLenum, data: *mut GLboolean) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_parameter(pname);
        unpack_parameter(cx, pname, data, value);
    })
}

#[unwind_aborts]
extern "system" fn get_buffer_parameteri64v(
    target: GLenum,
    pname: GLenum,
    params: *mut GLint64,
) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_buffer_parameter(target, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_buffer_parameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_buffer_parameter(target, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_buffer_pointerv(
    _target: GLenum,
    _pname: GLenum,
    _params: *const *mut c_void,
) -> () {
    unimplemented!("client-side buffers are not fully supported");
}

#[unwind_aborts]
extern "system" fn get_error() -> GLenum {
    with_context(|cx| {
        if cx.error_code != gl::NO_ERROR {
            mem::replace(&mut cx.error_code, gl::NO_ERROR)
        } else {
            cx.webgl.get_error()
        }
    })
}

#[unwind_aborts]
extern "system" fn get_floatv(pname: GLenum, data: *mut GLfloat) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_parameter(pname);
        unpack_parameter(cx, pname, data, value);
    })
}

#[unwind_aborts]
extern "system" fn get_frag_data_location(program: GLuint, name: *const GLchar) -> GLint {
    try_with_context(-1, |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let name = user_str(name)?;
        Ok(cx.webgl.get_frag_data_location(program, name))
    })
}

#[unwind_aborts]
extern "system" fn get_framebuffer_attachment_parameteriv(
    target: GLenum,
    attachment: GLenum,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    with_context(|cx| {
        let value = cx
            .webgl
            .get_framebuffer_attachment_parameter(target, attachment, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_integer64i_v(target: GLenum, index: GLuint, data: *mut GLint64) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_indexed_parameter(target, index);
        unpack_parameter(cx, target, data, value);
    })
}

#[unwind_aborts]
extern "system" fn get_integer64v(pname: GLenum, data: *mut GLint64) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_parameter(pname);
        unpack_parameter(cx, pname, data, value);
    })
}

#[unwind_aborts]
extern "system" fn get_integeri_v(target: GLenum, index: GLuint, data: *mut GLint) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_indexed_parameter(target, index);
        unpack_parameter(cx, target, data, value);
    })
}

#[unwind_aborts]
extern "system" fn get_integerv(pname: GLenum, data: *mut GLint) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_parameter(pname);
        unpack_parameter(cx, pname, data, value);
    })
}

#[unwind_aborts]
extern "system" fn get_internalformativ(
    target: GLenum,
    internalformat: GLenum,
    pname: GLenum,
    buf_size: GLsizei,
    params: *mut GLint,
) -> () {
    with_context(|cx| {
        let value = cx
            .webgl
            .get_internalformat_parameter(target, internalformat, pname);
        unpack_parameter_bounded(cx, pname, buf_size, ptr::null_mut(), params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_program_binary(
    _program: GLuint,
    _buf_size: GLsizei,
    _length: *mut GLsizei,
    _binary_format: *mut GLenum,
    _binary: *mut c_void,
) -> () {
    unimplemented!("shader binaries are not supported")
}

#[unwind_aborts]
extern "system" fn get_program_info_log(
    program: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    info_log: *mut GLchar,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        if let Some(log_src) = cx.webgl.get_program_info_log(program) {
            unpack_str(buf_size, length, info_log, &log_src);
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_programiv(program: GLuint, pname: GLenum, params: *mut GLint) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let value = match pname {
            gl::INFO_LOG_LENGTH => {
                ((cx.webgl.get_program_info_log(program).unwrap().len() + 1) as u32).into()
            }
            _ => cx.webgl.get_program_parameter(program, pname),
        };
        unpack_parameter(cx, pname, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_query_objectuiv(id: GLuint, pname: GLenum, params: *mut GLuint) -> () {
    try_with_context((), |cx| {
        let query = cx.queries.get(id)?;
        let value = cx.webgl.get_query_parameter(query, pname);
        unpack_parameter(cx, pname, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_queryiv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
    try_with_context((), |cx| {
        let params = user_mut(params);
        let query = cx.webgl.get_query(target, pname);
        if let Some(query) = query {
            *params = cx.queries.find(query) as GLint;
        } else {
            *params = 0;
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_renderbuffer_parameteriv(
    target: GLenum,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_renderbuffer_parameter(target, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_sampler_parameterfv(
    sampler: GLuint,
    pname: GLenum,
    params: *mut GLfloat,
) -> () {
    try_with_context((), |cx| {
        let sampler = cx.samplers.get(sampler)?;
        let value = cx.webgl.get_sampler_parameter(sampler, pname);
        unpack_parameter(cx, pname, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_sampler_parameteriv(
    sampler: GLuint,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    try_with_context((), |cx| {
        let sampler = cx.samplers.get(sampler)?;
        let value = cx.webgl.get_sampler_parameter(sampler, pname);
        unpack_parameter(cx, pname, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_shader_info_log(
    shader: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    info_log: *mut GLchar,
) -> () {
    try_with_context((), |cx| {
        let shader = cx.shaders.get(shader)?.as_shader()?;
        if let Some(log_src) = cx.webgl.get_shader_info_log(shader) {
            unpack_str(buf_size, length, info_log, &log_src);
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_shader_precision_format(
    shadertype: GLenum,
    precisiontype: GLenum,
    range: *mut GLint,
    precision: *mut GLint,
) -> () {
    with_context(|cx| {
        let range = user_array_mut(2, range);
        let precision = user_mut(precision);
        if let Some(format) = cx
            .webgl
            .get_shader_precision_format(shadertype, precisiontype)
        {
            range[0] = format.range_min();
            range[1] = format.range_max();
            *precision = format.precision();
        }
    })
}

#[unwind_aborts]
extern "system" fn get_shader_source(
    shader: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    source: *mut GLchar,
) -> () {
    try_with_context((), |cx| {
        let shader = cx.shaders.get(shader)?.as_shader()?;
        if let Some(source_src) = cx.webgl.get_shader_source(shader) {
            unpack_str(buf_size, length, source, &source_src);
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_shaderiv(shader: GLuint, pname: GLenum, params: *mut GLint) -> () {
    try_with_context((), |cx| {
        let shader = cx.shaders.get(shader)?.as_shader()?;
        let value = match pname {
            gl::INFO_LOG_LENGTH => (cx
                .webgl
                .get_shader_info_log(shader)
                .map(|s| s.len() + 1)
                .unwrap_or(0) as u32)
                .into(),
            gl::SHADER_SOURCE_LENGTH => (cx
                .webgl
                .get_shader_source(shader)
                .map(|s| s.len() + 1)
                .unwrap_or(0) as u32)
                .into(),
            _ => cx.webgl.get_shader_parameter(shader, pname),
        };
        unpack_parameter(cx, pname, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_string(name: GLenum) -> *const GLubyte {
    with_context(|cx| cache_string(cx.webgl.get_parameter(name)))
}

#[unwind_aborts]
extern "system" fn get_stringi(name: GLenum, index: GLuint) -> *const GLubyte {
    with_context(|cx| cache_string(cx.webgl.get_indexed_parameter(name, index)))
}

#[unwind_aborts]
extern "system" fn get_synciv(
    sync: GLsync,
    pname: GLenum,
    buf_size: GLsizei,
    length: *mut GLsizei,
    values: *mut GLint,
) -> () {
    try_with_context((), |cx| {
        let sync = cx.syncs.get(sync as GLuint)?;
        let value = cx.webgl.get_sync_parameter(sync, pname);
        unpack_parameter_bounded(cx, pname, buf_size, length, values, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_tex_parameterfv(target: GLenum, pname: GLenum, params: *mut GLfloat) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_tex_parameter(target, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_tex_parameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_tex_parameter(target, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_transform_feedback_varying(
    program: GLuint,
    index: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    size: *mut GLsizei,
    type_: *mut GLenum,
    name: *mut GLchar,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        if let Some(info) = cx.webgl.get_transform_feedback_varying(program, index) {
            unpack_active_info(buf_size, length, size, type_, name, info);
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_uniform_block_index(
    program: GLuint,
    uniform_block_name: *const GLchar,
) -> GLuint {
    try_with_context(gl::INVALID_INDEX, |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let name = user_str(uniform_block_name)?;
        Ok(cx.webgl.get_uniform_block_index(program, name))
    })
}

#[unwind_aborts]
extern "system" fn get_uniform_indices(
    program: GLuint,
    uniform_count: GLsizei,
    uniform_names: *const *const GLchar,
    uniform_indices: *mut GLuint,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let uniform_names: Vec<_> = user_array(uniform_count, uniform_names)
            .iter()
            .copied()
            .map(user_str)
            .collect::<Result<_, _>>()?;
        let uniform_indices = user_array_mut(uniform_count, uniform_indices);

        if let Some(indices_src) = cx.webgl.get_uniform_indices(program, &uniform_names) {
            uniform_indices.copy_from_slice(&indices_src);
        }
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_uniform_location(program: GLuint, name: *const GLchar) -> GLint {
    try_with_context(-1, |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let name = user_str(name)?;
        Ok(cx
            .uniforms
            .get_id(cx.webgl.get_uniform_location(program, name)))
    })
}

#[unwind_aborts]
extern "system" fn get_uniformfv(program: GLuint, location: GLint, params: *mut GLfloat) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_obj(program, location)?;
        let program = cx.shaders.get(program)?.as_program()?;
        let value = cx.webgl.get_uniform(program, location);
        unpack_parameter(cx, gl::NONE, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_uniformiv(program: GLuint, location: GLint, params: *mut GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_obj(program, location)?;
        let program = cx.shaders.get(program)?.as_program()?;
        let value = cx.webgl.get_uniform(program, location);
        unpack_parameter(cx, gl::NONE, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_uniformuiv(program: GLuint, location: GLint, params: *mut GLuint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_obj(program, location)?;
        let program = cx.shaders.get(program)?.as_program()?;
        let value = cx.webgl.get_uniform(program, location);
        unpack_parameter(cx, gl::NONE, params, value);
        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn get_vertex_attrib_iiv(index: GLuint, pname: GLenum, params: *mut GLint) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_vertex_attrib(index, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_vertex_attrib_iuiv(index: GLuint, pname: GLenum, params: *mut GLuint) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_vertex_attrib(index, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_vertex_attrib_pointerv(
    _index: GLuint,
    _pname: GLenum,
    _pointer: *const *mut c_void,
) -> () {
    unimplemented!("client-side buffers are not fully supported");
}

#[unwind_aborts]
extern "system" fn get_vertex_attribfv(index: GLuint, pname: GLenum, params: *mut GLfloat) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_vertex_attrib(index, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn get_vertex_attribiv(index: GLuint, pname: GLenum, params: *mut GLint) -> () {
    with_context(|cx| {
        let value = cx.webgl.get_vertex_attrib(index, pname);
        unpack_parameter(cx, pname, params, value);
    })
}

#[unwind_aborts]
extern "system" fn hint(target: GLenum, mode: GLenum) -> () {
    with_context(|cx| cx.webgl.hint(target, mode))
}

#[unwind_aborts]
extern "system" fn invalidate_framebuffer(
    target: GLenum,
    num_attachments: GLsizei,
    attachments: *const GLenum,
) -> () {
    let attachments = user_array(num_attachments, attachments);
    with_context(|cx| cx.webgl.invalidate_framebuffer(target, attachments))
}

#[unwind_aborts]
extern "system" fn invalidate_sub_framebuffer(
    target: GLenum,
    num_attachments: GLsizei,
    attachments: *const GLenum,
    x: GLint,
    y: GLint,
    width: GLsizei,
    height: GLsizei,
) -> () {
    let attachments = user_array(num_attachments, attachments);
    with_context(|cx| {
        cx.webgl
            .invalidate_sub_framebuffer(target, attachments, x, y, width, height)
    })
}

#[unwind_aborts]
extern "system" fn is_buffer(buffer: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.buffers
            .get_nullable(buffer)
            .map(|buffer| gl_boolean(cx.webgl.is_buffer(buffer)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_enabled(cap: GLenum) -> GLboolean {
    with_context(|cx| gl_boolean(cx.webgl.is_enabled(cap)))
}

#[unwind_aborts]
extern "system" fn is_framebuffer(framebuffer: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.framebuffers
            .get_nullable(framebuffer)
            .map(|fb| gl_boolean(cx.webgl.is_framebuffer(fb)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_program(program: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.shaders
            .get_nullable(program)
            .and_then(|opt| opt.map(ProgramOrShader::as_program).transpose())
            .map(|program| gl_boolean(cx.webgl.is_program(program)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_query(id: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.queries
            .get_nullable(id)
            .map(|query| gl_boolean(cx.webgl.is_query(query)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_renderbuffer(renderbuffer: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.renderbuffers
            .get_nullable(renderbuffer)
            .map(|rb| gl_boolean(cx.webgl.is_renderbuffer(rb)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_sampler(sampler: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.samplers
            .get_nullable(sampler)
            .map(|sampler| gl_boolean(cx.webgl.is_sampler(sampler)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_shader(shader: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.shaders
            .get_nullable(shader)
            .and_then(|opt| opt.map(ProgramOrShader::as_shader).transpose())
            .map(|shader| gl_boolean(cx.webgl.is_shader(shader)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_sync(sync: GLsync) -> GLboolean {
    with_context(|cx| {
        cx.syncs
            .get_nullable(sync as GLuint)
            .map(|sync| gl_boolean(cx.webgl.is_sync(sync)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_texture(texture: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.textures
            .get_nullable(texture)
            .map(|texture| gl_boolean(cx.webgl.is_texture(texture)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_transform_feedback(id: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.transform_feedbacks
            .get_nullable(id)
            .map(|tf| gl_boolean(cx.webgl.is_transform_feedback(tf)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn is_vertex_array(array: GLuint) -> GLboolean {
    with_context(|cx| {
        cx.vertex_arrays
            .get_nullable(array)
            .map(|va| gl_boolean(cx.webgl.is_vertex_array(va)))
            .unwrap_or(gl::FALSE)
    })
}

#[unwind_aborts]
extern "system" fn line_width(width: GLfloat) -> () {
    with_context(|cx| cx.webgl.line_width(width))
}

#[unwind_aborts]
extern "system" fn link_program(program: GLuint) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        Ok(cx.webgl.link_program(program))
    })
}

#[unwind_aborts]
extern "system" fn map_buffer_range(
    _target: GLenum,
    _offset: GLintptr,
    _length: GLsizeiptr,
    _access: GLbitfield,
) -> *mut c_void {
    unimplemented!("client-side buffers are not fully supported");
}

#[unwind_aborts]
extern "system" fn pause_transform_feedback() -> () {
    with_context(|cx| cx.webgl.pause_transform_feedback())
}

#[unwind_aborts]
extern "system" fn pixel_storei(pname: GLenum, param: GLint) -> () {
    with_context(|cx| cx.webgl.pixel_storei(pname, param))
}

#[unwind_aborts]
extern "system" fn polygon_offset(factor: GLfloat, units: GLfloat) -> () {
    with_context(|cx| cx.webgl.polygon_offset(factor, units))
}

#[unwind_aborts]
extern "system" fn program_binary(
    _program: GLuint,
    _binary_format: GLenum,
    _binary: *const c_void,
    _length: GLsizei,
) -> () {
    unimplemented!("shader binaries are not supported")
}

#[unwind_aborts]
extern "system" fn program_parameteri(_program: GLuint, _pname: GLenum, _value: GLint) -> () {
    unimplemented!("shader binaries are not supported")
}

#[unwind_aborts]
extern "system" fn read_buffer(src: GLenum) -> () {
    with_context(|cx| cx.webgl.read_buffer(src))
}

#[unwind_aborts]
extern "system" fn read_pixels(
    x: GLint,
    y: GLint,
    width: GLsizei,
    height: GLsizei,
    format: GLenum,
    type_: GLenum,
    pixels: *mut c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::PIXEL_PACK_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl.read_pixels_1(
                x,
                y,
                width,
                height,
                format,
                type_,
                pixels as webgl::GLintptr,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn release_shader_compiler() -> () {
    unimplemented!("shader binaries are not supported")
}

#[unwind_aborts]
extern "system" fn renderbuffer_storage(
    target: GLenum,
    internalformat: GLenum,
    width: GLsizei,
    height: GLsizei,
) -> () {
    with_context(|cx| {
        cx.webgl
            .renderbuffer_storage(target, internalformat, width, height)
    })
}

#[unwind_aborts]
extern "system" fn renderbuffer_storage_multisample(
    target: GLenum,
    samples: GLsizei,
    internalformat: GLenum,
    width: GLsizei,
    height: GLsizei,
) -> () {
    with_context(|cx| {
        cx.webgl
            .renderbuffer_storage_multisample(target, samples, internalformat, width, height)
    })
}

#[unwind_aborts]
extern "system" fn resume_transform_feedback() -> () {
    with_context(|cx| cx.webgl.resume_transform_feedback())
}

#[unwind_aborts]
extern "system" fn sample_coverage(value: GLfloat, invert: GLboolean) -> () {
    let invert = webgl_boolean(invert);
    with_context(|cx| cx.webgl.sample_coverage(value, invert))
}

#[unwind_aborts]
extern "system" fn sampler_parameterf(sampler: GLuint, pname: GLenum, param: GLfloat) -> () {
    try_with_context((), |cx| {
        let sampler = cx.samplers.get(sampler)?;
        Ok(cx.webgl.sampler_parameterf(sampler, pname, param))
    })
}

#[unwind_aborts]
extern "system" fn sampler_parameterfv(
    sampler: GLuint,
    pname: GLenum,
    param: *const GLfloat,
) -> () {
    let param = user_array(1, param);
    sampler_parameterf(sampler, pname, param[0])
}

#[unwind_aborts]
extern "system" fn sampler_parameteri(sampler: GLuint, pname: GLenum, param: GLint) -> () {
    try_with_context((), |cx| {
        let sampler = cx.samplers.get(sampler)?;
        Ok(cx.webgl.sampler_parameteri(sampler, pname, param))
    })
}

#[unwind_aborts]
extern "system" fn sampler_parameteriv(sampler: GLuint, pname: GLenum, param: *const GLint) -> () {
    let param = user_array(1, param);
    sampler_parameteri(sampler, pname, param[0])
}

#[unwind_aborts]
extern "system" fn scissor(x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
    with_context(|cx| cx.webgl.scissor(x, y, width, height))
}

#[unwind_aborts]
extern "system" fn shader_binary(
    _count: GLsizei,
    _shaders: *const GLuint,
    _binaryformat: GLenum,
    _binary: *const c_void,
    _length: GLsizei,
) -> () {
    unimplemented!("shader binaries are not supported")
}

#[unwind_aborts]
extern "system" fn shader_source(
    shader: GLuint,
    count: GLsizei,
    string: *const *const GLchar,
    length: *const GLint,
) -> () {
    try_with_context((), |cx| {
        let shader = cx.shaders.get(shader)?.as_shader()?;
        let string = user_array(count, string);
        let length = user_array_nullable(count, length);

        let mut source_buf = String::new();
        for i in 0..count as usize {
            let maybe_len = length.and_then(|slice| {
                let len = slice[i];
                if len < 0 {
                    None
                } else {
                    Some(len as GLsizei)
                }
            });

            let source_part;
            if let Some(len) = maybe_len {
                source_part = user_str_bounded(len, string[i])?;
            } else {
                source_part = user_str(string[i])?;
            }
            source_buf.push_str(source_part);
        }
        Ok(cx.webgl.shader_source(shader, &source_buf))
    })
}

#[unwind_aborts]
extern "system" fn stencil_func(func: GLenum, ref_: GLint, mask: GLuint) -> () {
    with_context(|cx| cx.webgl.stencil_func(func, ref_, mask))
}

#[unwind_aborts]
extern "system" fn stencil_func_separate(
    face: GLenum,
    func: GLenum,
    ref_: GLint,
    mask: GLuint,
) -> () {
    with_context(|cx| cx.webgl.stencil_func_separate(face, func, ref_, mask))
}

#[unwind_aborts]
extern "system" fn stencil_mask(mask: GLuint) -> () {
    with_context(|cx| cx.webgl.stencil_mask(mask))
}

#[unwind_aborts]
extern "system" fn stencil_mask_separate(face: GLenum, mask: GLuint) -> () {
    with_context(|cx| cx.webgl.stencil_mask_separate(face, mask))
}

#[unwind_aborts]
extern "system" fn stencil_op(fail: GLenum, zfail: GLenum, zpass: GLenum) -> () {
    with_context(|cx| cx.webgl.stencil_op(fail, zfail, zpass))
}

#[unwind_aborts]
extern "system" fn stencil_op_separate(
    face: GLenum,
    sfail: GLenum,
    dpfail: GLenum,
    dppass: GLenum,
) -> () {
    with_context(|cx| cx.webgl.stencil_op_separate(face, sfail, dpfail, dppass))
}

#[unwind_aborts]
extern "system" fn tex_image2_d(
    target: GLenum,
    level: GLint,
    internalformat: GLint,
    width: GLsizei,
    height: GLsizei,
    border: GLint,
    format: GLenum,
    type_: GLenum,
    pixels: *const c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::PIXEL_UNPACK_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl.tex_image2_d_2(
                target,
                level,
                internalformat,
                width,
                height,
                border,
                format,
                type_,
                pixels as webgl::GLintptr,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn tex_image3_d(
    target: GLenum,
    level: GLint,
    internalformat: GLint,
    width: GLsizei,
    height: GLsizei,
    depth: GLsizei,
    border: GLint,
    format: GLenum,
    type_: GLenum,
    pixels: *const c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::PIXEL_UNPACK_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl.tex_image3_d(
                target,
                level,
                internalformat,
                width,
                height,
                depth,
                border,
                format,
                type_,
                pixels as webgl::GLintptr,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn tex_parameterf(target: GLenum, pname: GLenum, param: GLfloat) -> () {
    with_context(|cx| cx.webgl.tex_parameterf(target, pname, param))
}

#[unwind_aborts]
extern "system" fn tex_parameterfv(target: GLenum, pname: GLenum, params: *const GLfloat) -> () {
    let params = user_array(1, params);
    tex_parameterf(target, pname, params[0]);
}

#[unwind_aborts]
extern "system" fn tex_parameteri(target: GLenum, pname: GLenum, param: GLint) -> () {
    with_context(|cx| cx.webgl.tex_parameteri(target, pname, param))
}

#[unwind_aborts]
extern "system" fn tex_parameteriv(target: GLenum, pname: GLenum, params: *const GLint) -> () {
    let params = user_array(1, params);
    tex_parameteri(target, pname, params[0]);
}

#[unwind_aborts]
extern "system" fn tex_storage2_d(
    target: GLenum,
    levels: GLsizei,
    internalformat: GLenum,
    width: GLsizei,
    height: GLsizei,
) -> () {
    with_context(|cx| {
        cx.webgl
            .tex_storage2_d(target, levels, internalformat, width, height)
    })
}

#[unwind_aborts]
extern "system" fn tex_storage3_d(
    target: GLenum,
    levels: GLsizei,
    internalformat: GLenum,
    width: GLsizei,
    height: GLsizei,
    depth: GLsizei,
) -> () {
    with_context(|cx| {
        cx.webgl
            .tex_storage3_d(target, levels, internalformat, width, height, depth)
    })
}

#[unwind_aborts]
extern "system" fn tex_sub_image2_d(
    target: GLenum,
    level: GLint,
    xoffset: GLint,
    yoffset: GLint,
    width: GLsizei,
    height: GLsizei,
    format: GLenum,
    type_: GLenum,
    pixels: *const c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::PIXEL_UNPACK_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl.tex_sub_image2_d_2(
                target,
                level,
                xoffset,
                yoffset,
                width,
                height,
                format,
                type_,
                pixels as webgl::GLintptr,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn tex_sub_image3_d(
    target: GLenum,
    level: GLint,
    xoffset: GLint,
    yoffset: GLint,
    zoffset: GLint,
    width: GLsizei,
    height: GLsizei,
    depth: GLsizei,
    format: GLenum,
    type_: GLenum,
    pixels: *const c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::PIXEL_UNPACK_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl.tex_sub_image3_d(
                target,
                level,
                xoffset,
                yoffset,
                zoffset,
                width,
                height,
                depth,
                format,
                type_,
                pixels as webgl::GLintptr,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn transform_feedback_varyings(
    program: GLuint,
    count: GLsizei,
    varyings: *const *const GLchar,
    buffer_mode: GLenum,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        let varyings: Vec<_> = user_array(count, varyings)
            .iter()
            .copied()
            .map(user_str)
            .collect::<Result<_, _>>()?;
        Ok(cx
            .webgl
            .transform_feedback_varyings(program, &varyings, buffer_mode))
    })
}

#[unwind_aborts]
extern "system" fn uniform1f(location: GLint, v0: GLfloat) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform1f(location, v0))
    })
}

#[unwind_aborts]
extern "system" fn uniform1fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(1 * count, value);
        Ok(cx.webgl.uniform1fv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform1i(location: GLint, v0: GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform1i(location, v0))
    })
}

#[unwind_aborts]
extern "system" fn uniform1iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(1 * count, value);
        Ok(cx.webgl.uniform1iv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform1ui(location: GLint, v0: GLuint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform1ui(location, v0))
    })
}

#[unwind_aborts]
extern "system" fn uniform1uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(1 * count, value);
        Ok(cx.webgl.uniform1uiv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform2f(location: GLint, v0: GLfloat, v1: GLfloat) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform2f(location, v0, v1))
    })
}

#[unwind_aborts]
extern "system" fn uniform2fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(2 * count, value);
        Ok(cx.webgl.uniform2fv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform2i(location: GLint, v0: GLint, v1: GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform2i(location, v0, v1))
    })
}

#[unwind_aborts]
extern "system" fn uniform2iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(2 * count, value);
        Ok(cx.webgl.uniform2iv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform2ui(location: GLint, v0: GLuint, v1: GLuint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform2ui(location, v0, v1))
    })
}

#[unwind_aborts]
extern "system" fn uniform2uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(2 * count, value);
        Ok(cx.webgl.uniform2uiv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform3f(location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform3f(location, v0, v1, v2))
    })
}

#[unwind_aborts]
extern "system" fn uniform3fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(3 * count, value);
        Ok(cx.webgl.uniform3fv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform3i(location: GLint, v0: GLint, v1: GLint, v2: GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform3i(location, v0, v1, v2))
    })
}

#[unwind_aborts]
extern "system" fn uniform3iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(3 * count, value);
        Ok(cx.webgl.uniform3iv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform3ui(location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform3ui(location, v0, v1, v2))
    })
}

#[unwind_aborts]
extern "system" fn uniform3uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(3 * count, value);
        Ok(cx.webgl.uniform3uiv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform4f(
    location: GLint,
    v0: GLfloat,
    v1: GLfloat,
    v2: GLfloat,
    v3: GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform4f(location, v0, v1, v2, v3))
    })
}

#[unwind_aborts]
extern "system" fn uniform4fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(4 * count, value);
        Ok(cx.webgl.uniform4fv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform4i(location: GLint, v0: GLint, v1: GLint, v2: GLint, v3: GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform4i(location, v0, v1, v2, v3))
    })
}

#[unwind_aborts]
extern "system" fn uniform4iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(4 * count, value);
        Ok(cx.webgl.uniform4iv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform4ui(
    location: GLint,
    v0: GLuint,
    v1: GLuint,
    v2: GLuint,
    v3: GLuint,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        Ok(cx.webgl.uniform4ui(location, v0, v1, v2, v3))
    })
}

#[unwind_aborts]
extern "system" fn uniform4uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let value = user_array(4 * count, value);
        Ok(cx.webgl.uniform4uiv(location, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_block_binding(
    program: GLuint,
    uniform_block_index: GLuint,
    uniform_block_binding: GLuint,
) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        Ok(cx
            .webgl
            .uniform_block_binding(program, uniform_block_index, uniform_block_binding))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix2fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(2 * 2 * count, value);
        Ok(cx.webgl.uniform_matrix2fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix2x3fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(2 * 3 * count, value);
        Ok(cx
            .webgl
            .uniform_matrix2x3fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix2x4fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(2 * 4 * count, value);
        Ok(cx
            .webgl
            .uniform_matrix2x4fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix3fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(3 * 3 * count, value);
        Ok(cx.webgl.uniform_matrix3fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix3x2fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(3 * 2 * count, value);
        Ok(cx
            .webgl
            .uniform_matrix3x2fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix3x4fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(3 * 4 * count, value);
        Ok(cx
            .webgl
            .uniform_matrix3x4fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix4fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(4 * 4 * count, value);
        Ok(cx.webgl.uniform_matrix4fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix4x2fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(4 * 2 * count, value);
        Ok(cx
            .webgl
            .uniform_matrix4x2fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn uniform_matrix4x3fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    try_with_context((), |cx| {
        let location = cx.uniforms.get_active_obj_nullable(location)?;
        let transpose = webgl_boolean(transpose);
        let value = user_array(4 * 3 * count, value);
        Ok(cx
            .webgl
            .uniform_matrix4x3fv(location, transpose, value, 0, 0))
    })
}

#[unwind_aborts]
extern "system" fn unmap_buffer(_target: GLenum) -> GLboolean {
    unimplemented!("client-side buffers are not fully supported");
}

#[unwind_aborts]
extern "system" fn use_program(program: GLuint) -> () {
    try_with_context((), |cx| {
        let program = if program == 0 {
            None
        } else {
            Some(cx.shaders.get(program)?.as_program()?)
        };
        cx.webgl.use_program(program);

        let current_program = match cx.webgl.get_parameter(gl::CURRENT_PROGRAM) {
            Value::Reference(reference) => cx
                .shaders
                .find(ProgramOrShader::Program(reference.try_into().unwrap())),
            Value::Null => 0,
            _ => unreachable!(),
        };
        cx.uniforms.using_program(current_program);

        Ok(())
    })
}

#[unwind_aborts]
extern "system" fn validate_program(program: GLuint) -> () {
    try_with_context((), |cx| {
        let program = cx.shaders.get(program)?.as_program()?;
        Ok(cx.webgl.validate_program(program))
    })
}

#[unwind_aborts]
extern "system" fn vertex_attrib1f(index: GLuint, x: GLfloat) -> () {
    with_context(|cx| cx.webgl.vertex_attrib1f(index, x))
}

#[unwind_aborts]
extern "system" fn vertex_attrib1fv(index: GLuint, v: *const GLfloat) -> () {
    let v = user_array(1, v);
    with_context(|cx| cx.webgl.vertex_attrib1fv(index, v))
}

#[unwind_aborts]
extern "system" fn vertex_attrib2f(index: GLuint, x: GLfloat, y: GLfloat) -> () {
    with_context(|cx| cx.webgl.vertex_attrib2f(index, x, y))
}

#[unwind_aborts]
extern "system" fn vertex_attrib2fv(index: GLuint, v: *const GLfloat) -> () {
    let v = user_array(2, v);
    with_context(|cx| cx.webgl.vertex_attrib2fv(index, v))
}

#[unwind_aborts]
extern "system" fn vertex_attrib3f(index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat) -> () {
    with_context(|cx| cx.webgl.vertex_attrib3f(index, x, y, z))
}

#[unwind_aborts]
extern "system" fn vertex_attrib3fv(index: GLuint, v: *const GLfloat) -> () {
    let v = user_array(3, v);
    with_context(|cx| cx.webgl.vertex_attrib3fv(index, v))
}

#[unwind_aborts]
extern "system" fn vertex_attrib4f(
    index: GLuint,
    x: GLfloat,
    y: GLfloat,
    z: GLfloat,
    w: GLfloat,
) -> () {
    with_context(|cx| cx.webgl.vertex_attrib4f(index, x, y, z, w))
}

#[unwind_aborts]
extern "system" fn vertex_attrib4fv(index: GLuint, v: *const GLfloat) -> () {
    let v = user_array(4, v);
    with_context(|cx| cx.webgl.vertex_attrib4fv(index, v))
}

#[unwind_aborts]
extern "system" fn vertex_attrib_divisor(index: GLuint, divisor: GLuint) -> () {
    with_context(|cx| cx.webgl.vertex_attrib_divisor(index, divisor))
}

#[unwind_aborts]
extern "system" fn vertex_attrib_i4i(index: GLuint, x: GLint, y: GLint, z: GLint, w: GLint) -> () {
    with_context(|cx| cx.webgl.vertex_attrib_i4i(index, x, y, z, w))
}

#[unwind_aborts]
extern "system" fn vertex_attrib_i4iv(index: GLuint, v: *const GLint) -> () {
    let v = user_array(4, v);
    with_context(|cx| cx.webgl.vertex_attrib_i4iv(index, v))
}

#[unwind_aborts]
extern "system" fn vertex_attrib_i4ui(
    index: GLuint,
    x: GLuint,
    y: GLuint,
    z: GLuint,
    w: GLuint,
) -> () {
    with_context(|cx| cx.webgl.vertex_attrib_i4ui(index, x, y, z, w))
}

#[unwind_aborts]
extern "system" fn vertex_attrib_i4uiv(index: GLuint, v: *const GLuint) -> () {
    let v = user_array(4, v);
    with_context(|cx| cx.webgl.vertex_attrib_i4uiv(index, v))
}

#[unwind_aborts]
extern "system" fn vertex_attrib_i_pointer(
    index: GLuint,
    size: GLint,
    type_: GLenum,
    stride: GLsizei,
    pointer: *const c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::ARRAY_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            cx.webgl.vertex_attrib_i_pointer(
                index,
                size,
                type_,
                stride,
                pointer as webgl::GLintptr,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn vertex_attrib_pointer(
    index: GLuint,
    size: GLint,
    type_: GLenum,
    normalized: GLboolean,
    stride: GLsizei,
    pointer: *const c_void,
) -> () {
    with_context(|cx| {
        let buffer = cx.webgl.get_parameter(gl::ARRAY_BUFFER_BINDING);
        if buffer.is_null() {
            unimplemented!("client-side buffers are not fully supported");
        } else {
            let normalized = webgl_boolean(normalized);
            cx.webgl.vertex_attrib_pointer(
                index,
                size,
                type_,
                normalized,
                stride,
                pointer as webgl::GLintptr,
            );
        }
    })
}

#[unwind_aborts]
extern "system" fn viewport(x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
    with_context(|cx| cx.webgl.viewport(x, y, width, height))
}

#[unwind_aborts]
extern "system" fn wait_sync(sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> () {
    try_with_context((), |cx| {
        let sync = cx.syncs.get(sync as GLuint)?;
        Ok(cx.webgl.wait_sync(sync, flags, timeout as webgl::GLint64))
    })
}
