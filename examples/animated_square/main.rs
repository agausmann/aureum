// Example adapted from "A basic 2D WebGL animation example"
// https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Basic_2D_animation_example

use aureum::{Api, ContextBuilder};
use gl::types::*;
use std::f32::consts::PI;
use std::ffi::{CStr, CString};
use std::{mem, ptr};
use stdweb::js;
use stdweb::unstable::TryInto;
use stdweb::web::html_element::CanvasElement;
use stdweb::web::{document, window, INode, IParentNode};

const WIDTH: GLint = 800;
const HEIGHT: GLint = 600;

fn main() {
    std::panic::set_hook(Box::new(|panic_info| {
        js!(console.error(@{format!("{}", panic_info)}));
    }));
    gl::load_with(aureum::get_proc_address);

    let canvas: CanvasElement = document()
        .create_element("canvas")
        .unwrap()
        .try_into()
        .unwrap();
    canvas.set_width(WIDTH as u32);
    canvas.set_height(HEIGHT as u32);
    document()
        .query_selector("body")
        .unwrap()
        .unwrap()
        .append_child(&canvas);

    let context = ContextBuilder::new()
        .gl_version(Api::Gles, (2, 0))
        .canvas(&canvas)
        .build()
        .expect("unable to create context");
    context.make_current();

    let shader_set = [
        (gl::VERTEX_SHADER, &include_bytes!("vertex.glsl")[..]),
        (gl::FRAGMENT_SHADER, &include_bytes!("fragment.glsl")[..]),
    ];
    let shader_program = build_shader_program(&shader_set);

    let u_scaling_factor;
    let u_global_color;
    let u_rotation_vector;
    let a_vertex_position;
    unsafe {
        gl::UseProgram(shader_program);
        u_scaling_factor = gl::GetUniformLocation(
            shader_program,
            CString::new("uScalingFactor").unwrap().as_ptr(),
        );
        u_global_color = gl::GetUniformLocation(
            shader_program,
            CString::new("uGlobalColor").unwrap().as_ptr(),
        );
        u_rotation_vector = gl::GetUniformLocation(
            shader_program,
            CString::new("uRotationVector").unwrap().as_ptr(),
        );
        a_vertex_position = gl::GetAttribLocation(
            shader_program,
            CString::new("aVertexPosition").unwrap().as_ptr(),
        ) as GLuint;
    }

    #[rustfmt::skip]
    let vertex_array: [GLfloat; 12] = [
        -0.5, 0.5,
        0.5, 0.5,
        0.5, -0.5,

        -0.5, 0.5,
        0.5, -0.5,
        -0.5, -0.5,
    ];
    let vertex_num_components = 2;
    let vertex_count = vertex_array.len() as GLsizei / vertex_num_components;

    let mut vertex_buffer = 0;
    unsafe {
        gl::GenBuffers(1, &mut vertex_buffer);
        gl::BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
        gl::BufferData(
            gl::ARRAY_BUFFER,
            mem::size_of_val(&vertex_array) as GLsizeiptr,
            vertex_array.as_ptr() as *const _,
            gl::STATIC_DRAW,
        );
    }

    let aspect_ratio = WIDTH as GLfloat / HEIGHT as GLfloat;
    let current_scale = [1.0, aspect_ratio];

    let previous_time = 0.0;
    let degrees_per_second = 90.0;

    let current_angle = 0.0;

    let user_data = Box::new(UserData {
        vertex_buffer,
        vertex_num_components,
        vertex_count,

        shader_program,
        u_scaling_factor,
        u_global_color,
        u_rotation_vector,
        a_vertex_position,

        current_angle,
        degrees_per_second,
        current_scale,
        previous_time,
    });
    animate_scene(Box::leak(user_data));
}

fn build_shader_program(shader_set: &[(GLenum, &[u8])]) -> GLuint {
    unsafe {
        let program = gl::CreateProgram();

        for &(shader_type, shader_source) in shader_set {
            let shader = compile_shader(shader_type, shader_source);

            if shader != 0 {
                gl::AttachShader(program, shader);
            }
        }

        gl::LinkProgram(program);
        let mut link_status = 0;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut link_status);

        if link_status == 0 {
            let mut info_log_length = 0;
            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut info_log_length);
            let mut info_log = vec![0; info_log_length as usize];
            gl::GetProgramInfoLog(
                program,
                info_log_length,
                ptr::null_mut(),
                info_log.as_mut_ptr() as *mut _,
            );
            js!(console.error(@{format!(
                "Error linking shader program:\n{}",
                CStr::from_bytes_with_nul(&info_log)
                    .unwrap()
                    .to_string_lossy()
            )}));
        }
        program
    }
}

fn compile_shader(shader_type: GLenum, shader_source: &[u8]) -> GLuint {
    unsafe {
        let shader_source = CString::new(shader_source).unwrap();

        let shader = gl::CreateShader(shader_type);
        gl::ShaderSource(shader, 1, [shader_source.as_ptr()].as_ptr(), ptr::null());

        gl::CompileShader(shader);
        let mut compile_status = 0;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut compile_status);

        if compile_status == 0 {
            let mut info_log_length = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut info_log_length);
            let mut info_log = vec![0; info_log_length as usize];
            gl::GetShaderInfoLog(
                shader,
                info_log_length,
                ptr::null_mut(),
                info_log.as_mut_ptr() as *mut _,
            );
            js!(console.error(@{format!(
                "Error compiling shader:\n{}",
                CStr::from_bytes_with_nul(&info_log)
                    .unwrap()
                    .to_string_lossy()
            )}));
        }
        shader
    }
}

struct UserData {
    vertex_buffer: GLuint,
    vertex_num_components: GLsizei,
    vertex_count: GLsizei,

    shader_program: GLuint,
    u_scaling_factor: GLint,
    u_rotation_vector: GLint,
    u_global_color: GLint,
    a_vertex_position: GLuint,

    current_angle: GLfloat,
    degrees_per_second: GLfloat,
    current_scale: [GLfloat; 2],
    previous_time: f64,
}

fn animate_scene(data_ptr: *mut UserData) {
    unsafe {
        let data = &mut *data_ptr;
        gl::Viewport(0, 0, WIDTH, HEIGHT);
        gl::ClearColor(0.8, 0.9, 1.0, 1.0);
        gl::Clear(gl::COLOR_BUFFER_BIT);

        let radians = data.current_angle * PI / 180.0;
        let current_rotation = [radians.sin(), radians.cos()];

        gl::UseProgram(data.shader_program);
        gl::Uniform2fv(data.u_scaling_factor, 1, data.current_scale.as_ptr());
        gl::Uniform2fv(data.u_rotation_vector, 1, current_rotation.as_ptr());
        gl::Uniform4fv(data.u_global_color, 1, [0.1, 0.7, 0.2, 1.0].as_ptr());

        gl::BindBuffer(gl::ARRAY_BUFFER, data.vertex_buffer);

        gl::EnableVertexAttribArray(data.a_vertex_position);
        gl::VertexAttribPointer(
            data.a_vertex_position,
            data.vertex_num_components,
            gl::FLOAT,
            gl::FALSE,
            0,
            0 as *const _,
        );

        gl::DrawArrays(gl::TRIANGLES, 0, data.vertex_count);
        window().request_animation_frame(move |current_time| {
            let delta_angle =
                ((current_time - data.previous_time) as GLfloat / 1000.0) * data.degrees_per_second;
            data.current_angle = (data.current_angle + delta_angle) % 360.0;
            data.previous_time = current_time;
            animate_scene(data_ptr)
        });
    }
}
