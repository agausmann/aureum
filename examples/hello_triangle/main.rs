// Example adapted from "Hello Triangle", available at the following URL:
// https://www.khronos.org/assets/uploads/books/openglr_es_20_programming_guide_sample.pdf

use aureum::Context;
use gl::types::*;
use std::ffi::{CStr, CString};
use std::{mem, ptr};
use stdweb::unstable::TryInto;
use stdweb::web::html_element::CanvasElement;
use stdweb::web::{document, INode, IParentNode};
use stdweb::{js, Reference};
use webgl_stdweb::WebGLRenderingContext;

struct UserData {
    program_object: GLuint,
    vertex_buffer: GLuint,
}

// Create a shader object, load the shader source, and compile the shader.
fn load_shader(shader_src: &CStr, type_: GLenum) -> GLuint {
    unsafe {
        // Create the shader object
        let shader = gl::CreateShader(type_);

        if shader == 0 {
            return 0;
        }

        // Load the shader source
        gl::ShaderSource(shader, 1, [shader_src.as_ptr()].as_ptr(), ptr::null());

        // Compile the shader
        gl::CompileShader(shader);

        // Check the compile status
        let mut compiled = 0;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut compiled);
        if compiled == 0 {
            let mut info_len = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut info_len);
            if info_len > 1 {
                let mut info_log = vec![0; info_len as usize];
                gl::GetShaderInfoLog(
                    shader,
                    info_len,
                    ptr::null_mut(),
                    info_log.as_mut_ptr() as *mut _,
                );
                js! {
                    console.error(@{format!(
                        "Error compiling shader:\n{}\n",
                        CStr::from_bytes_with_nul(&info_log).unwrap().to_string_lossy()
                    )})
                }
            }
            gl::DeleteShader(shader);
            return 0;
        }

        shader
    }
}

fn init() -> Option<UserData> {
    unsafe {
        let v_shader_str = CString::new(&include_bytes!("vertex.glsl")[..]).unwrap();
        let f_shader_str = CString::new(&include_bytes!("fragment.glsl")[..]).unwrap();

        // Load the vertex/fragment shaders
        let vertex_shader = load_shader(&v_shader_str, gl::VERTEX_SHADER);
        let fragment_shader = load_shader(&f_shader_str, gl::FRAGMENT_SHADER);

        // Create the program object
        let program_object = gl::CreateProgram();

        if program_object == 0 {
            return None;
        }

        gl::AttachShader(program_object, vertex_shader);
        gl::AttachShader(program_object, fragment_shader);

        // Bind vPosition to attribute 0
        gl::BindAttribLocation(
            program_object,
            0,
            CString::new("vPosition").unwrap().as_ptr(),
        );

        // Link the program
        gl::LinkProgram(program_object);

        // Check the link status
        let mut linked = 0;
        gl::GetProgramiv(program_object, gl::LINK_STATUS, &mut linked);
        if linked == 0 {
            let mut info_len = 0;
            gl::GetProgramiv(program_object, gl::INFO_LOG_LENGTH, &mut info_len);
            if info_len > 1 {
                let mut info_log = vec![0; info_len as usize];
                gl::GetProgramInfoLog(
                    program_object,
                    info_len,
                    ptr::null_mut(),
                    info_log.as_mut_ptr() as *mut _,
                );
                js! {
                    console.error(@{format!(
                        "Error linking program:\n{}\n",
                        CStr::from_bytes_with_nul(&info_log).unwrap().to_string_lossy()
                    )})
                }
            }
            gl::DeleteProgram(program_object);
            return None;
        }

        gl::ClearColor(0.0, 0.0, 0.0, 1.0);

        let mut vertex_buffer = 0;
        gl::GenBuffers(1, &mut vertex_buffer);

        Some(UserData {
            program_object,
            vertex_buffer,
        })
    }
}

const WIDTH: GLint = 800;
const HEIGHT: GLint = 600;

fn draw(user_data: &UserData) {
    unsafe {
        #[rustfmt::skip]
        let vertices: [f32; 9] = [
            0.0, 0.5, 0.0,
            -0.5, -0.5, 0.0,
            0.5, -0.5, 0.0,
        ];

        // Set the viewport
        gl::Viewport(0, 0, WIDTH, HEIGHT);

        // Clear the color buffer
        gl::Clear(gl::COLOR_BUFFER_BIT);

        // Use the program object
        gl::UseProgram(user_data.program_object);

        // Load the vertex data
        gl::BindBuffer(gl::ARRAY_BUFFER, user_data.vertex_buffer);
        gl::BufferData(
            gl::ARRAY_BUFFER,
            mem::size_of_val(&vertices) as GLsizeiptr,
            vertices.as_ptr() as *const _,
            gl::STREAM_DRAW,
        );
        gl::VertexAttribPointer(0, 3, gl::FLOAT, gl::FALSE, 0, 0 as *const _);
        gl::EnableVertexAttribArray(0);

        gl::DrawArrays(gl::TRIANGLES, 0, 3);
    }
}

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

    let webgl: WebGLRenderingContext = canvas.get_context().unwrap();
    let context = Context::new(Reference::from(webgl).try_into().unwrap());
    aureum::make_current(Some(context.clone()));

    let user_data = init().unwrap();

    draw(&user_data);
}
