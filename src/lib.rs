#![feature(box_patterns)]

extern crate lalrpop_util;
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

pub mod ast;
pub mod grammar;
pub mod eval;

#[wasm_bindgen]
pub fn eval_code(code: &str) -> String {
    match grammar::TermParser::new().parse(code) {
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
    use ast::Term::{Int, Var, App, Abs, Return};

    #[test]
    fn check_parse() {
        assert_eq!(
            grammar::TermParser::new().parse("fun x . 0").unwrap(),
            Box::new(Abs(
                "x".to_string(),
                Box::new(Int(0))))
        );
    }

    #[test]
    fn check_eval_base() {
        let mut context = eval::Context::empty();
        assert_eq!(
            eval::eval_step(&Int(3), &mut context).unwrap(),
            Int(3));

        context.push_var("x".to_string(), Int(3));
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
        // assert_eq!(eval::eval_ast(&id).unwrap(), id);

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
}
