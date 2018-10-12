use program::Program;
use wasm_bindgen::prelude::*;

pub static mut PROGRAM: Program = Program::new();

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(msg: &str);
}

// A macro to provide `println!(..)`-style syntax for `console.log` logging.
macro_rules! log {
    ($($t:tt)*) => (log(&format!($($t)*)))
}

#[wasm_bindgen]
pub fn eval_line(code: &str) -> String {
    unsafe { PROGRAM.eval_line(code) }
}

#[wasm_bindgen]
pub fn eval_program(code: &str) -> String {
    unsafe { PROGRAM.eval(code) }
}

#[wasm_bindgen]
pub fn set_typechecker(serialized: u8) {
    unsafe {
        PROGRAM.set_typechecker(match serialized {
            0b0000_0000 => {
                log!("switching to simple");
                ::typecheck::simple::typecheck
            }
            0b0000_0001 => {
                log!("switching to subtyping");
                ::typecheck::sub::typecheck
            }
            _ => unimplemented!(),
        })
    }
}
