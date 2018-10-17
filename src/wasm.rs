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

#[wasm_bindgen]
pub fn set_typechecker(serialized: u8) {
    unsafe {
        PROGRAM.set_typechecker(match serialized {
            0b0000_0000 => ::typecheck::simple::typecheck,
            0b0000_0001 => ::typecheck::sub::typecheck,
            0b0000_0010 => ::typecheck::hm::typecheck,
            0b0000_0100 => ::typecheck::sysf::typecheck,
            0b0000_0101 => ::typecheck::fsub::typecheck,
            _ => unimplemented!(),
        })
    }
}
