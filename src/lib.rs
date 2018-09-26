#![feature(box_patterns)]

extern crate lalrpop_util;
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

pub mod ast;
pub mod assoclist;
pub mod grammar;
pub mod eval;
pub mod typecheck;

#[wasm_bindgen]
pub fn eval_code(code: &str) -> String {
    match grammar::TermParser::new().parse(code) {
        Ok(ref ast) => {
            match typecheck::typecheck(ast, &mut typecheck::Context::empty()) {
                Ok(type_) => match eval::eval_ast(ast) {
                    Ok(result) => format!("{}: {}", result, type_),
                    Err(err) => err.to_string(),
                },
                Err(err) => err.to_string(),
            }
        },
        Err(err) => err.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::Term::*;
    use ast::ArithOp;

    fn eval_code(code: &str) -> String {
        match eval::eval_ast(&grammar::TermParser::new().parse(code).unwrap()) {
            Ok(ast) => ast.to_string(),
            Err(err) => err.to_string()
        }
    }

    fn typecheck_code(code: &str) -> String {
        match typecheck::typecheck(
            &grammar::TermParser::new().parse(code).unwrap(),
            &mut typecheck::Context::empty()) {
            Ok(type_) => type_.to_string(),
            Err(err) => err.to_string()
        }
    }

    #[test]
    fn check_parse() {
        assert_eq!(
            grammar::TermParser::new().parse("fun x: Bool . 0").unwrap(),
            Box::new(Abs(
                "x".to_string(),
                Box::new(typecheck::Type::Bool),
                Box::new(Int(0))))
        );
    }

    #[test]
    fn check_eval_base() {
        let mut context: eval::Context = assoclist::AssocList::empty();
        assert_eq!(
            eval::eval_step(&Int(3), &mut context).unwrap(),
            Int(3));

        context.push("x".to_string(), Int(3));
        assert_eq!(
            eval::eval_step(&Var("x".to_string()), &mut context).unwrap(),
            Int(3));
    }   

    #[test]
    fn check_eval_func() {
        let x = "x".to_string();
        let body = Box::new(Var(x.clone()));
        let id = Abs(
            x.clone(),
            Box::new(typecheck::Type::Int),
            body.clone());

        let app = App(Box::new(id.clone()), Box::new(Int(33)));
        assert_eq!(eval::eval_ast(&id).unwrap(), id);

        let mut context = eval::Context::empty();
        let mut curr = eval::eval_step(&app, &mut context).unwrap();
        assert_eq!(curr, Return(body));
        assert_eq!(*context.peek().unwrap(), (x.clone(), Int(33)));

        curr = eval::eval_step(&curr, &mut context).unwrap();
        assert_eq!(curr, Return(Box::new(Int(33))));
        assert_eq!(*context.peek().unwrap(), (x.clone(), Int(33)));

        curr = eval::eval_step(&curr, &mut context).unwrap();
        assert_eq!(curr, Int(33));
        assert!(context.peek().is_none());

        assert_eq!(eval::eval_ast(&app).unwrap(), Int(33));
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

    #[test]
    fn ifelse() {
        assert_eq!(
            eval::eval_ast(&If(
                Box::new(Bool(true)),
                Box::new(Int(3)),
                Box::new(Int(5))
                )
            ).unwrap(),
            Int(3)
        )
    }

    #[test]
    fn e2e_eval() {
        assert_eq!(eval_code("if (3 % 2) = 1 then 10 else 2"), "10");
        assert_eq!(eval_code("1 + 2 + 3 + 4"), "10");
        assert_eq!(eval_code("(fun x: Int . x*4 + 3) 3"), "15");
        assert_eq!(eval_code("let x := 5 in x"), "5");
        assert_eq!(eval_code("{a: 1, b: 2}"), "{a: 1, b: 2}");
        assert_eq!(eval_code("{a: 2}.a"), "2");
        assert_eq!(eval_code("{a: 2}.b"), "eval error: key b does not exist in record");
    }

    #[test]
    fn e2e_type() {
        assert_eq!(typecheck_code("if true then 0 else 2"), "Int");
        assert_eq!(
            typecheck_code("let x := 0 in if x + 2 then 0 else 2"),
            "If/else condition must be a Bool");
        assert_eq!(typecheck_code("fun x: Int . x >= 0"),
            "Int -> Bool");
        assert_eq!(typecheck_code("3 4"), "Tried to apply non function type");
        assert_eq!(typecheck_code("(fun x: Int . x*2 + 1) 0"), "Int");
    }
}
