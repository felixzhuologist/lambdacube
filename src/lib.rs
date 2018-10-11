#![feature(box_patterns)]

extern crate lalrpop_util;
extern crate wasm_bindgen;

pub mod assoclist;
pub mod grammar;
pub mod errors;
pub mod syntax;
pub mod simple;
pub mod sub;

use wasm_bindgen::prelude::*;
use errors::{EvalError, TypeError};
use syntax::{Term, Type};

#[wasm_bindgen]
pub fn eval_simple(code: &str) -> String {
    simple::Simple::new().eval_code(code)
}

#[wasm_bindgen]
pub fn eval_sub(code: &str) -> String {
    sub::Sub::new().eval_code(code)
}

pub trait TypeSystem {
    fn parse(&self, code: &str) -> Result<Box<Term>, String>;
    fn eval(&self, ast: &Term) -> Result<Term, EvalError>;
    fn typecheck(&self, ast: &Term) -> Result<Type, TypeError>;
    fn eval_code(&self, code: &str) -> String {
        self.parse(code)
            .and_then(|ref ast| {
                self.typecheck(ast)
                    .map_err(|e| e.to_string())
                    .and_then(|type_| self.eval(ast)
                        .map(|val| format!("{}: {}", val, type_))
                        .map_err(|e| e.to_string()))
            })
            .unwrap_or_else(|err_msg| err_msg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syntax::Term::*;
    use syntax::ArithOp;

    #[test]
    fn check_parse() {
        assert_eq!(
            grammar::TermParser::new().parse("fun x: Bool . 0").unwrap(),
            Box::new(Abs(
                "x".to_string(),
                Box::new(Type::Bool),
                Box::new(Int(0))))
        );
    }

    #[test]
    fn check_arith_parse() {
        assert_eq!(
            grammar::TermParser::new().parse("1 + 2 + 3").unwrap(),
            Box::new(Arith(
                Box::new(Int(1)),
                ArithOp::Add,
                Box::new(Arith(
                    Box::new(Int(2)),
                    ArithOp::Add,
                    Box::new(Int(3))
                    ))
                )));

       assert_eq!(
            grammar::TermParser::new().parse("1 * 2 + 3").unwrap(),
            Box::new(Arith(
                Box::new(Arith(
                    Box::new(Int(1)),
                    ArithOp::Mul,
                    Box::new(Int(2))
                    )),
                ArithOp::Add,
                Box::new(Int(3)),
                )));
    }
}
