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
            0b0000_0000 => ::typecheck::simple::typecheck_top,
            0b0000_0001 => ::typecheck::sub::typecheck_top,
            0b0000_0010 => ::typecheck::hm::typecheck_top,
            0b0000_0100 => ::typecheck::sysf::typecheck_top,
            0b0000_0101 => ::typecheck::fsub::typecheck_top,
            0b0000_1000 => ::typecheck::omega::typecheck,
            0b0000_1100 => ::typecheck::fomega::typecheck,
            0b0000_1101 => ::typecheck::fomsub::typecheck,
            // TODO: remove the possibility of subtyping and higher order only
            0b0000_1001 => ::typecheck::fomsub::typecheck,
            0b0010_0000 => ::typecheck::substructural::typecheck_top,
            _ => unimplemented!(),
        })
    }
}
