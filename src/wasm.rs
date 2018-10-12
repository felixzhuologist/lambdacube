use program::Program;
use wasm_bindgen::prelude::*;

pub static mut PROGRAM: Program = Program::new();

#[wasm_bindgen]
pub fn eval_line(code: &str) -> String {
    unsafe { PROGRAM.eval_line(code) }
}

#[wasm_bindgen]
pub fn eval_program(code: &str) -> String {
    unsafe { PROGRAM.eval(code) }
}
