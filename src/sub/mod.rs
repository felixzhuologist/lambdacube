pub mod typecheck;

use assoclist::TypeContext;
use errors::{EvalError, TypeError};
use syntax::{Term, Type};
use TypeSystem;

pub struct Sub {}

impl Sub {
    pub fn new() -> Sub {
        Sub {}
    }
}

impl TypeSystem for Sub {
    fn parse(&self, code: &str) -> Result<Box<Term>, String> {
        ::grammar::TermParser::new()
            .parse(code)
            .map_err(|e| e.to_string())
    }
    fn eval(&self, ast: &Term) -> Result<Term, EvalError> {
        ::simple::eval::eval_ast(ast)
    }
    fn typecheck(&self, ast: &Term) -> Result<Type, TypeError> {
        typecheck::typecheck(ast, &mut TypeContext::empty())
    }
}
