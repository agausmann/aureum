use std::cell::{Ref, RefCell};
use std::ffi::CStr;
use std::os::raw::*;
use std::rc::Rc;
use std::{mem, slice};

use gl::types::*;
use unwind_aborts::unwind_aborts;
use webgl_stdweb as webgl;
use webgl_stdweb::{
    GLContext, WebGLBuffer, WebGLFramebuffer, WebGLProgram, WebGLQuery, WebGLRenderbuffer,
    WebGLSampler, WebGLShader, WebGLTexture, WebGLTransformFeedback, WebGLVertexArrayObject,
};

pub struct Context {
    inner: Rc<RefCell<ContextInner>>,
}

struct ContextInner {
    webgl: GLContext,
    error_code: GLenum,
    shaders: ObjectMap<ProgramOrShader>,
    buffers: ObjectMap<WebGLBuffer>,
    framebuffers: ObjectMap<WebGLFramebuffer>,
    queries: ObjectMap<WebGLQuery>,
    renderbuffers: ObjectMap<WebGLRenderbuffer>,
    samplers: ObjectMap<WebGLSampler>,
    textures: ObjectMap<WebGLTexture>,
    transform_feedbacks: ObjectMap<WebGLTransformFeedback>,
    vertex_arrays: ObjectMap<WebGLVertexArrayObject>,
}

struct ObjectMap<T> {
    objects: Vec<Option<T>>,
}

impl<T> ObjectMap<T> {
    fn new() -> ObjectMap<T> {
        ObjectMap {
            objects: vec![None],
        }
    }

    fn get(&self, id: GLuint) -> Result<&T, GLenum> {
        self.objects
            .get(id as usize)
            .map(Option::as_ref)
            .flatten()
            .ok_or(gl::INVALID_VALUE)
    }

    fn get_nullable(&self, id: GLuint) -> Result<Option<&T>, GLenum> {
        if id == 0 {
            Ok(None)
        } else {
            self.get(id).map(Some)
        }
    }

    fn add(&mut self, obj: Option<T>) -> GLuint {
        if obj.is_some() {
            for (id, slot) in self.objects.iter_mut().enumerate().skip(1) {
                if slot.is_none() {
                    *slot = obj;
                }
                return id as GLuint;
            }
            let id = self.objects.len();
            self.objects.push(obj);
            id as GLuint
        } else {
            0
        }
    }

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

enum ProgramOrShader {
    Program(WebGLProgram),
    Shader(WebGLShader),
}

impl ProgramOrShader {
    fn as_program(&self) -> Result<&WebGLProgram, GLenum> {
        match self {
            ProgramOrShader::Program(program) => Ok(program),
            _ => Err(gl::INVALID_OPERATION),
        }
    }

    fn as_shader(&self) -> Result<&WebGLShader, GLenum> {
        match self {
            ProgramOrShader::Shader(shader) => Ok(shader),
            _ => Err(gl::INVALID_OPERATION),
        }
    }

    fn to_program(self) -> Result<WebGLProgram, GLenum> {
        match self {
            ProgramOrShader::Program(program) => Ok(program),
            _ => Err(gl::INVALID_OPERATION),
        }
    }

    fn to_shader(self) -> Result<WebGLShader, GLenum> {
        match self {
            ProgramOrShader::Shader(shader) => Ok(shader),
            _ => Err(gl::INVALID_OPERATION),
        }
    }
}

thread_local! {
    static CURRENT_CONTEXT: RefCell<Option<Context>> = RefCell::new(None);
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
    unsafe { CStr::from_ptr(data).to_str().ok().ok_or(gl::INVALID_VALUE) }
}

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
        _ => std::ptr::null(),
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
    todo!()
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
        cx.shaders.add(program.map(ProgramOrShader::Program))
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
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn draw_elements_instanced(
    mode: GLenum,
    count: GLsizei,
    type_: GLenum,
    indices: *const c_void,
    instancecount: GLsizei,
) -> () {
    todo!()
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
    todo!()
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
    todo!()
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
    target: GLenum,
    offset: GLintptr,
    length: GLsizeiptr,
) -> () {
    todo!()
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
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn get_active_uniform_block_name(
    program: GLuint,
    uniform_block_index: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    uniform_block_name: *mut GLchar,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_active_uniform_blockiv(
    program: GLuint,
    uniform_block_index: GLuint,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_active_uniformsiv(
    program: GLuint,
    uniform_count: GLsizei,
    uniform_indices: *const GLuint,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_attached_shaders(
    program: GLuint,
    max_count: GLsizei,
    count: *mut GLsizei,
    shaders: *mut GLuint,
) -> () {
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn get_buffer_parameteri64v(
    target: GLenum,
    pname: GLenum,
    params: *mut GLint64,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_buffer_parameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_buffer_pointerv(
    target: GLenum,
    pname: GLenum,
    params: *const *mut c_void,
) -> () {
    todo!()
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
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn get_integer64i_v(target: GLenum, index: GLuint, data: *mut GLint64) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_integer64v(pname: GLenum, data: *mut GLint64) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_integeri_v(target: GLenum, index: GLuint, data: *mut GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_integerv(pname: GLenum, data: *mut GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_internalformativ(
    target: GLenum,
    internalformat: GLenum,
    pname: GLenum,
    buf_size: GLsizei,
    params: *mut GLint,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_program_binary(
    program: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    binary_format: *mut GLenum,
    binary: *mut c_void,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_program_info_log(
    program: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    info_log: *mut GLchar,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_programiv(program: GLuint, pname: GLenum, params: *mut GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_query_objectuiv(id: GLuint, pname: GLenum, params: *mut GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_queryiv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_renderbuffer_parameteriv(
    target: GLenum,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_sampler_parameterfv(
    sampler: GLuint,
    pname: GLenum,
    params: *mut GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_sampler_parameteriv(
    sampler: GLuint,
    pname: GLenum,
    params: *mut GLint,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_shader_info_log(
    shader: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    info_log: *mut GLchar,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_shader_precision_format(
    shadertype: GLenum,
    precisiontype: GLenum,
    range: *mut GLint,
    precision: *mut GLint,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_shader_source(
    shader: GLuint,
    buf_size: GLsizei,
    length: *mut GLsizei,
    source: *mut GLchar,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_shaderiv(shader: GLuint, pname: GLenum, params: *mut GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_string(name: GLenum) -> *const GLubyte {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_stringi(name: GLenum, index: GLuint) -> *const GLubyte {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_synciv(
    sync: GLsync,
    pname: GLenum,
    buf_size: GLsizei,
    length: *mut GLsizei,
    values: *mut GLint,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_tex_parameterfv(target: GLenum, pname: GLenum, params: *mut GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_tex_parameteriv(target: GLenum, pname: GLenum, params: *mut GLint) -> () {
    todo!()
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
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn get_uniform_location(program: GLuint, name: *const GLchar) -> GLint {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_uniformfv(program: GLuint, location: GLint, params: *mut GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_uniformiv(program: GLuint, location: GLint, params: *mut GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_uniformuiv(program: GLuint, location: GLint, params: *mut GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_vertex_attrib_iiv(index: GLuint, pname: GLenum, params: *mut GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_vertex_attrib_iuiv(index: GLuint, pname: GLenum, params: *mut GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_vertex_attrib_pointerv(
    index: GLuint,
    pname: GLenum,
    pointer: *const *mut c_void,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_vertex_attribfv(index: GLuint, pname: GLenum, params: *mut GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn get_vertex_attribiv(index: GLuint, pname: GLenum, params: *mut GLint) -> () {
    todo!()
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
    todo!()
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
    target: GLenum,
    offset: GLintptr,
    length: GLsizeiptr,
    access: GLbitfield,
) -> *mut c_void {
    todo!()
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
    program: GLuint,
    binary_format: GLenum,
    binary: *const c_void,
    length: GLsizei,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn program_parameteri(program: GLuint, pname: GLenum, value: GLint) -> () {
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn release_shader_compiler() -> () {
    todo!()
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
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn scissor(x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
    with_context(|cx| cx.webgl.scissor(x, y, width, height))
}

#[unwind_aborts]
extern "system" fn shader_binary(
    count: GLsizei,
    shaders: *const GLuint,
    binaryformat: GLenum,
    binary: *const c_void,
    length: GLsizei,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn shader_source(
    shader: GLuint,
    count: GLsizei,
    string: *const *const GLchar,
    length: *const GLint,
) -> () {
    todo!()
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
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn tex_parameterf(target: GLenum, pname: GLenum, param: GLfloat) -> () {
    with_context(|cx| cx.webgl.tex_parameterf(target, pname, param))
}

#[unwind_aborts]
extern "system" fn tex_parameterfv(target: GLenum, pname: GLenum, params: *const GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn tex_parameteri(target: GLenum, pname: GLenum, param: GLint) -> () {
    with_context(|cx| cx.webgl.tex_parameteri(target, pname, param))
}

#[unwind_aborts]
extern "system" fn tex_parameteriv(target: GLenum, pname: GLenum, params: *const GLint) -> () {
    todo!()
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
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn transform_feedback_varyings(
    program: GLuint,
    count: GLsizei,
    varyings: *const *const GLchar,
    buffer_mode: GLenum,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform1f(location: GLint, v0: GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform1fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform1i(location: GLint, v0: GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform1iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform1ui(location: GLint, v0: GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform1uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform2f(location: GLint, v0: GLfloat, v1: GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform2fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform2i(location: GLint, v0: GLint, v1: GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform2iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform2ui(location: GLint, v0: GLuint, v1: GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform2uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform3f(location: GLint, v0: GLfloat, v1: GLfloat, v2: GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform3fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform3i(location: GLint, v0: GLint, v1: GLint, v2: GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform3iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform3ui(location: GLint, v0: GLuint, v1: GLuint, v2: GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform3uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform4f(
    location: GLint,
    v0: GLfloat,
    v1: GLfloat,
    v2: GLfloat,
    v3: GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform4fv(location: GLint, count: GLsizei, value: *const GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform4i(location: GLint, v0: GLint, v1: GLint, v2: GLint, v3: GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform4iv(location: GLint, count: GLsizei, value: *const GLint) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform4ui(
    location: GLint,
    v0: GLuint,
    v1: GLuint,
    v2: GLuint,
    v3: GLuint,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform4uiv(location: GLint, count: GLsizei, value: *const GLuint) -> () {
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform_matrix2x3fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform_matrix2x4fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform_matrix3fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform_matrix3x2fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform_matrix3x4fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform_matrix4fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform_matrix4x2fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn uniform_matrix4x3fv(
    location: GLint,
    count: GLsizei,
    transpose: GLboolean,
    value: *const GLfloat,
) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn unmap_buffer(target: GLenum) -> GLboolean {
    todo!()
}

#[unwind_aborts]
extern "system" fn use_program(program: GLuint) -> () {
    try_with_context((), |cx| {
        let program = if program == 0 {
            None
        } else {
            Some(cx.shaders.get(program)?.as_program()?)
        };
        Ok(cx.webgl.use_program(program))
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
    todo!()
}

#[unwind_aborts]
extern "system" fn vertex_attrib2f(index: GLuint, x: GLfloat, y: GLfloat) -> () {
    with_context(|cx| cx.webgl.vertex_attrib2f(index, x, y))
}

#[unwind_aborts]
extern "system" fn vertex_attrib2fv(index: GLuint, v: *const GLfloat) -> () {
    todo!()
}

#[unwind_aborts]
extern "system" fn vertex_attrib3f(index: GLuint, x: GLfloat, y: GLfloat, z: GLfloat) -> () {
    with_context(|cx| cx.webgl.vertex_attrib3f(index, x, y, z))
}

#[unwind_aborts]
extern "system" fn vertex_attrib3fv(index: GLuint, v: *const GLfloat) -> () {
    todo!()
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
    todo!()
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
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn vertex_attrib_i_pointer(
    index: GLuint,
    size: GLint,
    type_: GLenum,
    stride: GLsizei,
    pointer: *const c_void,
) -> () {
    todo!()
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
    todo!()
}

#[unwind_aborts]
extern "system" fn viewport(x: GLint, y: GLint, width: GLsizei, height: GLsizei) -> () {
    with_context(|cx| cx.webgl.viewport(x, y, width, height))
}

#[unwind_aborts]
extern "system" fn wait_sync(sync: GLsync, flags: GLbitfield, timeout: GLuint64) -> () {
    todo!()
}
