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

mod gles;

use std::borrow::Cow;
use std::os::raw::*;
use std::{fmt, ptr, str};

use stdweb::unstable::TryInto;
use stdweb::web::html_element::CanvasElement;
use stdweb::web::{document, IParentNode};
use webgl_stdweb::{GLContext, WebGL2RenderingContext, WebGLRenderingContext};

use crate::gles::ContextHandle;

/// The OpenGL APIs supported by this library.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Api {
    /// OpenGL ES
    Gles,
}

/// Builder-style constructor for [`Context`]s.
#[derive(Debug, Clone)]
pub struct ContextBuilder {
    api: Api,
    version: (u8, u8),
    canvas: Option<CanvasSelector>,
}

impl ContextBuilder {
    /// Creates a [`ContextBuilder`] with the default values:
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

    /// Sets the target canvas to the given [`CanvasElement`].
    pub fn canvas(&mut self, canvas: &CanvasElement) -> &mut Self {
        self.canvas = Some(CanvasSelector::Explicit(canvas.clone()));
        self
    }

    /// Attempts to build a [`Context`] from this configuration.
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
/// To construct [`Context`] instances, see [`ContextBuilder`].
#[derive(Clone)]
pub struct Context {
    handle: ContextHandle,
}

impl Context {
    /// Creates a new context from a given WebGL context.
    pub(crate) fn new(webgl: GLContext) -> Self {
        Self {
            handle: ContextHandle::new(webgl),
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
            .field(&format_args!("{:p}", self.handle))
            .finish()
    }
}

impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl Eq for Context {}

/// Returns whether the given context handle points to the current context.
///
/// If `context` is `None`, returns `true` if no context is active.
pub fn is_current(context: Option<&Context>) -> bool {
    gles::is_current(context.map(|cx| &cx.handle))
}

/// Makes the given context current. If `context` is `None`, the current context is unset.
pub fn make_current(context: Option<Context>) {
    gles::make_current(context.map(|cx| cx.handle))
}

/// Returns a pointer to a function given its name, or a null pointer if the named function is not
/// available. Designed to be passed to the `gl` crate like
/// `gl::load_with(aureum::get_proc_address)`.
pub fn get_proc_address(name: &str) -> *const c_void {
    match name {
        "glActiveTexture" => gles::active_texture as *const _,
        "glAttachShader" => gles::attach_shader as *const _,
        "glBeginQuery" => gles::begin_query as *const _,
        "glBeginTransformFeedback" => gles::begin_transform_feedback as *const _,
        "glBindAttribLocation" => gles::bind_attrib_location as *const _,
        "glBindBuffer" => gles::bind_buffer as *const _,
        "glBindBufferBase" => gles::bind_buffer_base as *const _,
        "glBindBufferRange" => gles::bind_buffer_range as *const _,
        "glBindFramebuffer" => gles::bind_framebuffer as *const _,
        "glBindRenderbuffer" => gles::bind_renderbuffer as *const _,
        "glBindSampler" => gles::bind_sampler as *const _,
        "glBindTexture" => gles::bind_texture as *const _,
        "glBindTransformFeedback" => gles::bind_transform_feedback as *const _,
        "glBindVertexArray" => gles::bind_vertex_array as *const _,
        "glBlendColor" => gles::blend_color as *const _,
        "glBlendEquation" => gles::blend_equation as *const _,
        "glBlendEquationSeparate" => gles::blend_equation_separate as *const _,
        "glBlendFunc" => gles::blend_func as *const _,
        "glBlendFuncSeparate" => gles::blend_func_separate as *const _,
        "glBlitFramebuffer" => gles::blit_framebuffer as *const _,
        "glBufferData" => gles::buffer_data as *const _,
        "glBufferSubData" => gles::buffer_sub_data as *const _,
        "glCheckFramebufferStatus" => gles::check_framebuffer_status as *const _,
        "glClear" => gles::clear as *const _,
        "glClearBufferfi" => gles::clear_bufferfi as *const _,
        "glClearBufferfv" => gles::clear_bufferfv as *const _,
        "glClearBufferiv" => gles::clear_bufferiv as *const _,
        "glClearBufferuiv" => gles::clear_bufferuiv as *const _,
        "glClearColor" => gles::clear_color as *const _,
        "glClearDepthf" => gles::clear_depthf as *const _,
        "glClearStencil" => gles::clear_stencil as *const _,
        "glClientWaitSync" => gles::client_wait_sync as *const _,
        "glColorMask" => gles::color_mask as *const _,
        "glCompileShader" => gles::compile_shader as *const _,
        "glCompressedTexImage2D" => gles::compressed_tex_image2_d as *const _,
        "glCompressedTexImage3D" => gles::compressed_tex_image3_d as *const _,
        "glCompressedTexSubImage2D" => gles::compressed_tex_sub_image2_d as *const _,
        "glCompressedTexSubImage3D" => gles::compressed_tex_sub_image3_d as *const _,
        "glCopyBufferSubData" => gles::copy_buffer_sub_data as *const _,
        "glCopyTexImage2D" => gles::copy_tex_image2_d as *const _,
        "glCopyTexSubImage2D" => gles::copy_tex_sub_image2_d as *const _,
        "glCopyTexSubImage3D" => gles::copy_tex_sub_image3_d as *const _,
        "glCreateProgram" => gles::create_program as *const _,
        "glCreateShader" => gles::create_shader as *const _,
        "glCullFace" => gles::cull_face as *const _,
        "glDeleteBuffers" => gles::delete_buffers as *const _,
        "glDeleteFramebuffers" => gles::delete_framebuffers as *const _,
        "glDeleteProgram" => gles::delete_program as *const _,
        "glDeleteQueries" => gles::delete_queries as *const _,
        "glDeleteRenderbuffers" => gles::delete_renderbuffers as *const _,
        "glDeleteSamplers" => gles::delete_samplers as *const _,
        "glDeleteShader" => gles::delete_shader as *const _,
        "glDeleteSync" => gles::delete_sync as *const _,
        "glDeleteTextures" => gles::delete_textures as *const _,
        "glDeleteTransformFeedbacks" => gles::delete_transform_feedbacks as *const _,
        "glDeleteVertexArrays" => gles::delete_vertex_arrays as *const _,
        "glDepthFunc" => gles::depth_func as *const _,
        "glDepthMask" => gles::depth_mask as *const _,
        "glDepthRangef" => gles::depth_rangef as *const _,
        "glDetachShader" => gles::detach_shader as *const _,
        "glDisable" => gles::disable as *const _,
        "glDisableVertexAttribArray" => gles::disable_vertex_attrib_array as *const _,
        "glDrawArrays" => gles::draw_arrays as *const _,
        "glDrawArraysInstanced" => gles::draw_arrays_instanced as *const _,
        "glDrawBuffers" => gles::draw_buffers as *const _,
        "glDrawElements" => gles::draw_elements as *const _,
        "glDrawElementsInstanced" => gles::draw_elements_instanced as *const _,
        "glDrawRangeElements" => gles::draw_range_elements as *const _,
        "glEnable" => gles::enable as *const _,
        "glEnableVertexAttribArray" => gles::enable_vertex_attrib_array as *const _,
        "glEndQuery" => gles::end_query as *const _,
        "glEndTransformFeedback" => gles::end_transform_feedback as *const _,
        "glFenceSync" => gles::fence_sync as *const _,
        "glFinish" => gles::finish as *const _,
        "glFlush" => gles::flush as *const _,
        "glFlushMappedBufferRange" => gles::flush_mapped_buffer_range as *const _,
        "glFramebufferRenderbuffer" => gles::framebuffer_renderbuffer as *const _,
        "glFramebufferTexture2D" => gles::framebuffer_texture2_d as *const _,
        "glFramebufferTextureLayer" => gles::framebuffer_texture_layer as *const _,
        "glFrontFace" => gles::front_face as *const _,
        "glGenBuffers" => gles::gen_buffers as *const _,
        "glGenFramebuffers" => gles::gen_framebuffers as *const _,
        "glGenQueries" => gles::gen_queries as *const _,
        "glGenRenderbuffers" => gles::gen_renderbuffers as *const _,
        "glGenSamplers" => gles::gen_samplers as *const _,
        "glGenTextures" => gles::gen_textures as *const _,
        "glGenTransformFeedbacks" => gles::gen_transform_feedbacks as *const _,
        "glGenVertexArrays" => gles::gen_vertex_arrays as *const _,
        "glGenerateMipmap" => gles::generate_mipmap as *const _,
        "glGetActiveAttrib" => gles::get_active_attrib as *const _,
        "glGetActiveUniform" => gles::get_active_uniform as *const _,
        "glGetActiveUniformBlockName" => gles::get_active_uniform_block_name as *const _,
        "glGetActiveUniformBlockiv" => gles::get_active_uniform_blockiv as *const _,
        "glGetActiveUniformsiv" => gles::get_active_uniformsiv as *const _,
        "glGetAttachedShaders" => gles::get_attached_shaders as *const _,
        "glGetAttribLocation" => gles::get_attrib_location as *const _,
        "glGetBooleanv" => gles::get_booleanv as *const _,
        "glGetBufferParameteri64v" => gles::get_buffer_parameteri64v as *const _,
        "glGetBufferParameteriv" => gles::get_buffer_parameteriv as *const _,
        "glGetBufferPointerv" => gles::get_buffer_pointerv as *const _,
        "glGetError" => gles::get_error as *const _,
        "glGetFloatv" => gles::get_floatv as *const _,
        "glGetFragDataLocation" => gles::get_frag_data_location as *const _,
        "glGetFramebufferAttachmentParameteriv" => {
            gles::get_framebuffer_attachment_parameteriv as *const _
        }
        "glGetInteger64i_v" => gles::get_integer64i_v as *const _,
        "glGetInteger64v" => gles::get_integer64v as *const _,
        "glGetIntegeri_v" => gles::get_integeri_v as *const _,
        "glGetIntegerv" => gles::get_integerv as *const _,
        "glGetInternalformativ" => gles::get_internalformativ as *const _,
        "glGetProgramBinary" => gles::get_program_binary as *const _,
        "glGetProgramInfoLog" => gles::get_program_info_log as *const _,
        "glGetProgramiv" => gles::get_programiv as *const _,
        "glGetQueryObjectuiv" => gles::get_query_objectuiv as *const _,
        "glGetQueryiv" => gles::get_queryiv as *const _,
        "glGetRenderbufferParameteriv" => gles::get_renderbuffer_parameteriv as *const _,
        "glGetSamplerParameterfv" => gles::get_sampler_parameterfv as *const _,
        "glGetSamplerParameteriv" => gles::get_sampler_parameteriv as *const _,
        "glGetShaderInfoLog" => gles::get_shader_info_log as *const _,
        "glGetShaderPrecisionFormat" => gles::get_shader_precision_format as *const _,
        "glGetShaderSource" => gles::get_shader_source as *const _,
        "glGetShaderiv" => gles::get_shaderiv as *const _,
        "glGetString" => gles::get_string as *const _,
        "glGetStringi" => gles::get_stringi as *const _,
        "glGetSynciv" => gles::get_synciv as *const _,
        "glGetTexParameterfv" => gles::get_tex_parameterfv as *const _,
        "glGetTexParameteriv" => gles::get_tex_parameteriv as *const _,
        "glGetTransformFeedbackVarying" => gles::get_transform_feedback_varying as *const _,
        "glGetUniformBlockIndex" => gles::get_uniform_block_index as *const _,
        "glGetUniformIndices" => gles::get_uniform_indices as *const _,
        "glGetUniformLocation" => gles::get_uniform_location as *const _,
        "glGetUniformfv" => gles::get_uniformfv as *const _,
        "glGetUniformiv" => gles::get_uniformiv as *const _,
        "glGetUniformuiv" => gles::get_uniformuiv as *const _,
        "glGetVertexAttribIiv" => gles::get_vertex_attrib_iiv as *const _,
        "glGetVertexAttribIuiv" => gles::get_vertex_attrib_iuiv as *const _,
        "glGetVertexAttribPointerv" => gles::get_vertex_attrib_pointerv as *const _,
        "glGetVertexAttribfv" => gles::get_vertex_attribfv as *const _,
        "glGetVertexAttribiv" => gles::get_vertex_attribiv as *const _,
        "glHint" => gles::hint as *const _,
        "glInvalidateFramebuffer" => gles::invalidate_framebuffer as *const _,
        "glInvalidateSubFramebuffer" => gles::invalidate_sub_framebuffer as *const _,
        "glIsBuffer" => gles::is_buffer as *const _,
        "glIsEnabled" => gles::is_enabled as *const _,
        "glIsFramebuffer" => gles::is_framebuffer as *const _,
        "glIsProgram" => gles::is_program as *const _,
        "glIsQuery" => gles::is_query as *const _,
        "glIsRenderbuffer" => gles::is_renderbuffer as *const _,
        "glIsSampler" => gles::is_sampler as *const _,
        "glIsShader" => gles::is_shader as *const _,
        "glIsSync" => gles::is_sync as *const _,
        "glIsTexture" => gles::is_texture as *const _,
        "glIsTransformFeedback" => gles::is_transform_feedback as *const _,
        "glIsVertexArray" => gles::is_vertex_array as *const _,
        "glLineWidth" => gles::line_width as *const _,
        "glLinkProgram" => gles::link_program as *const _,
        "glMapBufferRange" => gles::map_buffer_range as *const _,
        "glPauseTransformFeedback" => gles::pause_transform_feedback as *const _,
        "glPixelStorei" => gles::pixel_storei as *const _,
        "glPolygonOffset" => gles::polygon_offset as *const _,
        "glProgramBinary" => gles::program_binary as *const _,
        "glProgramParameteri" => gles::program_parameteri as *const _,
        "glReadBuffer" => gles::read_buffer as *const _,
        "glReadPixels" => gles::read_pixels as *const _,
        "glReleaseShaderCompiler" => gles::release_shader_compiler as *const _,
        "glRenderbufferStorage" => gles::renderbuffer_storage as *const _,
        "glRenderbufferStorageMultisample" => gles::renderbuffer_storage_multisample as *const _,
        "glResumeTransformFeedback" => gles::resume_transform_feedback as *const _,
        "glSampleCoverage" => gles::sample_coverage as *const _,
        "glSamplerParameterf" => gles::sampler_parameterf as *const _,
        "glSamplerParameterfv" => gles::sampler_parameterfv as *const _,
        "glSamplerParameteri" => gles::sampler_parameteri as *const _,
        "glSamplerParameteriv" => gles::sampler_parameteriv as *const _,
        "glScissor" => gles::scissor as *const _,
        "glShaderBinary" => gles::shader_binary as *const _,
        "glShaderSource" => gles::shader_source as *const _,
        "glStencilFunc" => gles::stencil_func as *const _,
        "glStencilFuncSeparate" => gles::stencil_func_separate as *const _,
        "glStencilMask" => gles::stencil_mask as *const _,
        "glStencilMaskSeparate" => gles::stencil_mask_separate as *const _,
        "glStencilOp" => gles::stencil_op as *const _,
        "glStencilOpSeparate" => gles::stencil_op_separate as *const _,
        "glTexImage2D" => gles::tex_image2_d as *const _,
        "glTexImage3D" => gles::tex_image3_d as *const _,
        "glTexParameterf" => gles::tex_parameterf as *const _,
        "glTexParameterfv" => gles::tex_parameterfv as *const _,
        "glTexParameteri" => gles::tex_parameteri as *const _,
        "glTexParameteriv" => gles::tex_parameteriv as *const _,
        "glTexStorage2D" => gles::tex_storage2_d as *const _,
        "glTexStorage3D" => gles::tex_storage3_d as *const _,
        "glTexSubImage2D" => gles::tex_sub_image2_d as *const _,
        "glTexSubImage3D" => gles::tex_sub_image3_d as *const _,
        "glTransformFeedbackVaryings" => gles::transform_feedback_varyings as *const _,
        "glUniform1f" => gles::uniform1f as *const _,
        "glUniform1fv" => gles::uniform1fv as *const _,
        "glUniform1i" => gles::uniform1i as *const _,
        "glUniform1iv" => gles::uniform1iv as *const _,
        "glUniform1ui" => gles::uniform1ui as *const _,
        "glUniform1uiv" => gles::uniform1uiv as *const _,
        "glUniform2f" => gles::uniform2f as *const _,
        "glUniform2fv" => gles::uniform2fv as *const _,
        "glUniform2i" => gles::uniform2i as *const _,
        "glUniform2iv" => gles::uniform2iv as *const _,
        "glUniform2ui" => gles::uniform2ui as *const _,
        "glUniform2uiv" => gles::uniform2uiv as *const _,
        "glUniform3f" => gles::uniform3f as *const _,
        "glUniform3fv" => gles::uniform3fv as *const _,
        "glUniform3i" => gles::uniform3i as *const _,
        "glUniform3iv" => gles::uniform3iv as *const _,
        "glUniform3ui" => gles::uniform3ui as *const _,
        "glUniform3uiv" => gles::uniform3uiv as *const _,
        "glUniform4f" => gles::uniform4f as *const _,
        "glUniform4fv" => gles::uniform4fv as *const _,
        "glUniform4i" => gles::uniform4i as *const _,
        "glUniform4iv" => gles::uniform4iv as *const _,
        "glUniform4ui" => gles::uniform4ui as *const _,
        "glUniform4uiv" => gles::uniform4uiv as *const _,
        "glUniformBlockBinding" => gles::uniform_block_binding as *const _,
        "glUniformMatrix2fv" => gles::uniform_matrix2fv as *const _,
        "glUniformMatrix2x3fv" => gles::uniform_matrix2x3fv as *const _,
        "glUniformMatrix2x4fv" => gles::uniform_matrix2x4fv as *const _,
        "glUniformMatrix3fv" => gles::uniform_matrix3fv as *const _,
        "glUniformMatrix3x2fv" => gles::uniform_matrix3x2fv as *const _,
        "glUniformMatrix3x4fv" => gles::uniform_matrix3x4fv as *const _,
        "glUniformMatrix4fv" => gles::uniform_matrix4fv as *const _,
        "glUniformMatrix4x2fv" => gles::uniform_matrix4x2fv as *const _,
        "glUniformMatrix4x3fv" => gles::uniform_matrix4x3fv as *const _,
        "glUnmapBuffer" => gles::unmap_buffer as *const _,
        "glUseProgram" => gles::use_program as *const _,
        "glValidateProgram" => gles::validate_program as *const _,
        "glVertexAttrib1f" => gles::vertex_attrib1f as *const _,
        "glVertexAttrib1fv" => gles::vertex_attrib1fv as *const _,
        "glVertexAttrib2f" => gles::vertex_attrib2f as *const _,
        "glVertexAttrib2fv" => gles::vertex_attrib2fv as *const _,
        "glVertexAttrib3f" => gles::vertex_attrib3f as *const _,
        "glVertexAttrib3fv" => gles::vertex_attrib3fv as *const _,
        "glVertexAttrib4f" => gles::vertex_attrib4f as *const _,
        "glVertexAttrib4fv" => gles::vertex_attrib4fv as *const _,
        "glVertexAttribDivisor" => gles::vertex_attrib_divisor as *const _,
        "glVertexAttribI4i" => gles::vertex_attrib_i4i as *const _,
        "glVertexAttribI4iv" => gles::vertex_attrib_i4iv as *const _,
        "glVertexAttribI4ui" => gles::vertex_attrib_i4ui as *const _,
        "glVertexAttribI4uiv" => gles::vertex_attrib_i4uiv as *const _,
        "glVertexAttribIPointer" => gles::vertex_attrib_i_pointer as *const _,
        "glVertexAttribPointer" => gles::vertex_attrib_pointer as *const _,
        "glViewport" => gles::viewport as *const _,
        "glWaitSync" => gles::wait_sync as *const _,
        _ => ptr::null(),
    }
}
