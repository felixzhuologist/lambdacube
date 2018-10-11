pub mod eval;
pub mod typecheck;

use ::assoclist::TypeContext;
use ::errors::{EvalError, TypeError};
use ::syntax::{Term, Type};
use ::TypeSystem;

pub struct Simple { }

impl Simple {
    pub fn new() -> Simple {
        Simple {}
    }
}

impl TypeSystem for Simple {
    fn parse(&self, code: &str) -> Result<Box<Term>, String> {
        ::grammar::TermParser::new().parse(code).map_err(|e| e.to_string())
    }
    fn eval(&self, ast: &Term) -> Result<Term, EvalError> {
        eval::eval_ast(ast)
    }
    fn typecheck(&self, ast: &Term) -> Result<Type, TypeError> {
        typecheck::typecheck(ast, &mut TypeContext::empty())
    }
}
