extern crate lalrpop_util;
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

pub mod ast;
pub mod grammar;
pub mod eval;

#[wasm_bindgen]
pub fn eval_code(code: &str) -> String {
    match grammar::ExprParser::new().parse(code) {
        Ok(ref ast) => eval::eval_ast(ast).to_string(),
        Err(err) => err.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let expr = grammar::ExprParser::new()
            .parse("22 * 44 + 66")
            .unwrap();
        assert_eq!(eval::eval_ast(&expr), 1034);
    }
}