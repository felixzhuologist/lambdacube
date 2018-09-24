#![feature(box_patterns)]

extern crate lalrpop_util;
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

pub mod ast;
pub mod assoclist;
pub mod grammar;
pub mod eval;

#[wasm_bindgen]
pub fn eval_code(code: &str) -> String {
    match grammar::ExprParser::new().parse(code) {
        Ok(ref ast) => match eval::eval_ast(ast) {
            Ok(result) => result.to_string(),
            Err(err) => err.to_string()
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
        eval::eval_ast(&grammar::ExprParser::new().parse(code).unwrap())
            .unwrap().to_string()
    }

    #[test]
    fn check_parse() {
        assert_eq!(
            grammar::ExprParser::new().parse("fun x . 0").unwrap(),
            Box::new(Abs(
                "x".to_string(),
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
        let id = Abs(x.clone(), body.clone());

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
            grammar::ExprParser::new().parse("1 + 2 + 3").unwrap(),
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
            grammar::ExprParser::new().parse("1 * 2 + 3").unwrap(),
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
    fn oneliners() {
        assert_eq!(eval_code("if (3 % 2) = 1 then 10 else 2"), "10");
        assert_eq!(eval_code("1 + 2 + 3 + 4"), "10");
        assert_eq!(eval_code("(fun x . x*4 + 3) 3"), "15");
    }
}
