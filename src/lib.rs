#![feature(box_patterns)]

extern crate lalrpop_util;
extern crate wasm_bindgen;

pub mod assoclist;
pub mod errors;
pub mod grammar;
pub mod simple;
pub mod sub;
pub mod syntax;

use errors::{EvalError, TypeError};
use syntax::{Term, Type};
use wasm_bindgen::prelude::*;

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
                self.typecheck(ast).map_err(|e| e.to_string()).and_then(
                    |type_| {
                        self.eval(ast)
                            .map(|val| format!("{}: {}", val, type_))
                            .map_err(|e| e.to_string())
                    },
                )
            }).unwrap_or_else(|err_msg| err_msg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::AssocList;
    use syntax::{ArithOp, Binder};
    use syntax::Term::*;

    #[test]
    fn check_parse() {
        assert_eq!(
            grammar::TermParser::new().parse("fun x: Bool . 0").unwrap(),
            Box::new(Abs(
                "x".to_string(),
                Box::new(Type::Bool),
                Box::new(Int(0))
            ))
        );

        assert_eq!(
            grammar::TermParser::new().parse("{a=2, b=true}").unwrap(),
            Box::new(Record(AssocList::from_vec(vec![
                ("a".into(), Box::new(Int(2))),
                ("b".into(), Box::new(Bool(true)))
            ])))
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
            ))
        );

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
            ))
        );
    }

    #[test]
    fn check_type_parse() {
        assert_eq!(
            grammar::TypeParser::new().parse("Int").unwrap(),
            Box::new(Type::Int)
        );

        assert_eq!(
            grammar::TypeParser::new().parse("Int -> Bool").unwrap(),
            Box::new(Type::Arr(Box::new(Type::Int), Box::new(Type::Bool)))
        );

        assert_eq!(
            grammar::TypeParser::new()
                .parse("Int -> (Bool -> Bool)")
                .unwrap(),
            Box::new(Type::Arr(
                Box::new(Type::Int),
                Box::new(Type::Arr(Box::new(Type::Bool), Box::new(Type::Bool)))
            ))
        );

        assert_eq!(
            grammar::TypeParser::new()
                .parse("{a: Int, b: Bool}")
                .unwrap(),
            Box::new(Type::Record(AssocList::from_vec(vec![
                ("a".into(), Box::new(Type::Int)),
                ("b".into(), Box::new(Type::Bool)),
            ])))
        );
    }

    #[test]
    fn check_binder_parse() {
        assert_eq!(
            grammar::BinderParser::new().parse("type IntFunc = Int -> Int").unwrap(),
            Binder::TyBind(
                "IntFunc".into(),
                Type::Arr(Box::new(Type::Int), Box::new(Type::Int))
            )
        );

        assert_eq!(
            grammar::BinderParser::new().parse("let myrec = {a=2}").unwrap(),
            Binder::VarBind(
                "myrec".into(),
                Record(AssocList::from_vec(vec![("a".into(), Box::new(Int(2)))]))
            )
        );
    }
}
