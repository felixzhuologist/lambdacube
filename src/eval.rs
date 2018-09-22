use std::fmt;

use ast::ArithOp;
use ast::BoolOp;
use ast::Term;
use ast::Term::{Arith, Abs, App, Bool, Int, Logic, Not, Var, Return};

#[derive(Debug)]
pub enum EvalError {
    NotFound(String)
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EvalError::NotFound(ref s) =>
                write!(f, "eval error: variable {} not found", s)
        }
    }
}

/// implementation of a context using an association list
pub struct Context {
    inner: Vec<(String, Term)>
}

impl Context {
    pub fn empty() -> Context {
        Context { inner: Vec::new() }
    }

    pub fn push_var(&mut self, name: String, val: Term) {
        self.inner.push((name, val));
    }

    pub fn pop_var(&mut self) {
        self.inner.pop();
    }

    pub fn peek(&self) -> Option<&(String, Term)> {
        self.inner.last()
    }

    pub fn lookup(&self, var: &str) -> Result<Term, EvalError> {
        for (name, val) in self.inner.iter().rev() {
            if name == var {
                return Ok(val.clone());
            }
        }
        Err(EvalError::NotFound(var.to_string()))
    }
}

/// call eval step on a term until it is stuck
pub fn eval_ast(term: &Term) -> Result<Term, EvalError> {
    let mut current = term.clone();
    let mut context = Context::empty();

    loop {
        let next = eval_step(&current, &mut context)?;
        if current == next {
            return Ok(current);
        }
        current = next;
    }
}

pub fn eval_step(term: &Term, context: &mut Context) -> Result<Term, EvalError> {
    match term {
        Not(box Bool(b)) => { Ok(Bool(!b)) },
        Not(box t) => { Ok(Not(Box::new(eval_step(t, context)?))) },
        App(box Abs(argname, body), box arg) if arg.is_reduced() => {
            context.push_var(argname.clone(), arg.clone());
            Ok(Return(body.clone()))
        },
        App(func, box arg) if func.is_val() => {
            Ok(App(func.clone(), Box::new(eval_step(arg, context)?)))
        },
        App(box func, arg) => {
            Ok(App(Box::new(eval_step(func, context)?), arg.clone()))
        },
        Return(box term) if (*term).is_val() => {
            context.pop_var();
            Ok(term.clone())
        },
        Return(box term) => {
            Ok(Return(Box::new(eval_step(term, context)?)))
        },
        Var(s) => { context.lookup(s) },
        Arith(box Int(a), op, box Int(b)) => {
            match op {
                ArithOp::Mul => Ok(Int(a * b)),
                ArithOp::Div => Ok(Int(a / b)),
                ArithOp::Add => Ok(Int(a + b)),
                ArithOp::Sub => Ok(Int(a - b)),
            }
        },
        Arith(left @ box Int(_), op, box right) => {
            Ok(Arith(left.clone(), op.clone(), Box::new(eval_step(right, context)?)))
        },
        Arith(box left, op, right) => {
            Ok(Arith(Box::new(eval_step(left, context)?), op.clone(), right.clone()))
        }
        Logic(box Bool(a), op, box Bool(b)) => {
            match op {
                BoolOp::And => Ok(Bool(*a && *b)),
                BoolOp::Or => Ok(Bool(*a || *b)),
            }
        },
        Logic(left @ box Bool(_), op, box right) => {
            Ok(Logic(left.clone(), op.clone(), Box::new(eval_step(right, context)?)))
        },
        Logic(box left, op, right) => {
            Ok(Logic(Box::new(eval_step(left, context)?), op.clone(), right.clone()))
        }
        _ => Ok(term.clone())
    }
}