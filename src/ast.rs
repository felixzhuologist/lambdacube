use assoclist::AssocList;
use typecheck::Type;

use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Bool(bool),
    Not(Box<Term>),
    Var(String),
    Int(i32),
    Abs(String, Box<Type>, Box<Term>),
    App(Box<Term>, Box<Term>),
    // when performing a substitution, we wrap the body in a Return to let any
    // AST manipulators know when to pop from the context
    Return(Box<Term>),
    Arith(Box<Term>, ArithOp, Box<Term>),
    Logic(Box<Term>, BoolOp, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(String, Box<Term>, Box<Term>),
    Record(AssocList<String, Box<Term>>),
    Proj(Box<Term>, String),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ArithOp {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    Eq_,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BoolOp {
    And,
    Or
}

impl Term {
    pub fn is_reduced(&self) -> bool {
        match self {
            Term::App(_, _) => false,
            _ => true
        }
    }

    pub fn is_val(&self) -> bool {
        match self {
            Term::Int(_) => true,
            Term::Bool(_) => true,
            Term::Not(box t) => t.is_val(),
            Term::Abs(_, _, _) => true,
            Term::Record(fields) =>
                fields.inner.iter().all(|(_, val)| val.is_val()),
            _ => false,
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Bool(b) => write!(f, "{}", b),
            Term::Not(ref t) => write!(f, "not {}", t),
            Term::Var(ref s) => write!(f, "{}", s),
            Term::Int(n) => write!(f, "{}", n),
            Term::Abs(ref argname, ref ty, ref body) =>
                write!(f, "fun {}: {} . {}", argname, ty, body),
            Term::App(ref func, ref arg) =>
                write!(f, "{} {}", func, arg),
            Term::Return(ref term) => write!(f, "{}", term),
            Term::Arith(ref l, ref op, ref r) => write!(f, "{} {} {}", l, op, r),
            Term::Logic(ref l, ref op, ref r) => write!(f, "{} {} {}", l, op, r),
            Term::If(ref cond, ref t1, ref t2) =>
                write!(f, "if {} then {} else {}", cond, t1, t2),
            Term::Let(ref x, ref val, ref term) =>
                write!(f, "let {} := {} in {}", x, val, term),
            Term::Record(ref rec) => write!(f, "{{{}}}", rec.inner.iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<String>>()
                .join(", ")),
            Term::Proj(ref t, ref attr) => write!(f, "{}.{}", t, attr)
        }
    }
}

impl fmt::Display for ArithOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ArithOp::Mul => write!(f, "*"),
            ArithOp::Div => write!(f, "/"),
            ArithOp::Add => write!(f, "+"),
            ArithOp::Sub => write!(f, "-"),
            ArithOp::Mod => write!(f, "%"),
            ArithOp::Eq_ => write!(f, "="),
            ArithOp::Neq => write!(f, "≠"),
            ArithOp::Gt => write!(f, ">"),
            ArithOp::Lt => write!(f, "<"),
            ArithOp::Gte => write!(f, "≥"),
            ArithOp::Lte => write!(f, "≤"),
        }
    }
}

impl fmt::Display for BoolOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BoolOp::And => write!(f, "and"),
            BoolOp::Or => write!(f, "or"),
        }
    }
}
