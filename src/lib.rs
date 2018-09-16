extern crate lalrpop_util;
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

pub mod ast;
pub mod grammar;
pub mod eval;

#[wasm_bindgen]
pub fn eval_code(code: &str) -> i32 {
    let expr = grammar::ExprParser::new().parse(code).unwrap();
    eval::eval_(&expr)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let expr = grammar::ExprParser::new()
            .parse("22 * 44 + 66")
            .unwrap();
        assert_eq!(eval::eval_(&expr), 1034);
    }
}