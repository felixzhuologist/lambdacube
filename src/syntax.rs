use assoclist::{AssocList, TypeContext};

use std::fmt;
use std::marker;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Command {
    Binder(Binder),
    Term(Box<Term>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Binder {
    VarBind(String, Term),
    TyBind(String, Type),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Bool(bool),
    Not(Box<Term>),
    Var(String),
    Int(i32),
    Abs(String, Box<Type>, Box<Term>),
    /// type abstraction (function from type to term)
    TyAbs(String, Box<Term>),
    /// regular abstraction with inferred type
    InfAbs(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    TyApp(Box<Term>, Box<Type>),
    Arith(Box<Term>, ArithOp, Box<Term>),
    Logic(Box<Term>, BoolOp, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(String, Box<Term>, Box<Term>),
    Record(AssocList<String, Box<Term>>),
    Proj(Box<Term>, String),
}

impl Term {
    // TODO: there is probably a better way to do this
    pub fn is_reduced(&self) -> bool {
        match self {
            Term::App(_, _) => false,
            _ => true,
        }
    }

    pub fn is_val(&self) -> bool {
        match self {
            Term::Int(_)
            | Term::Bool(_)
            | Term::Abs(_, _, _)
            | Term::TyAbs(_, _)
            | Term::InfAbs(_, _) => true,
            Term::Not(box t) => t.is_val(),
            Term::Record(fields) => {
                fields.inner.iter().all(|(_, val)| val.is_val())
            }
            Term::App(_, _)
            | Term::TyApp(_, _)
            | Term::Var(_)
            | Term::Arith(_, _, _)
            | Term::Logic(_, _, _)
            | Term::If(_, _, _)
            | Term::Let(_, _, _)
            | Term::Proj(_, _) => false,
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
            Term::Abs(_, _, _) | Term::InfAbs(_, _) | Term::TyAbs(_, _) => {
                write!(f, "<fun>")
            }
            Term::App(ref func, ref arg) => write!(f, "{} {}", func, arg),
            Term::TyApp(ref func, ref arg) => write!(f, "{} [{}]", func, arg),
            Term::Arith(ref l, ref op, ref r) => {
                write!(f, "{} {} {}", l, op, r)
            }
            Term::Logic(ref l, ref op, ref r) => {
                write!(f, "{} {} {}", l, op, r)
            }
            Term::If(ref cond, ref t1, ref t2) => {
                write!(f, "if {} then {} else {}", cond, t1, t2)
            }
            Term::Let(ref x, ref val, ref term) => {
                write!(f, "let {} = {} in {}", x, val, term)
            }
            Term::Record(ref rec) => write!(
                f,
                "{{{}}}",
                rec.inner
                    .iter()
                    .map(|(k, v)| format!("{}={}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Term::Proj(ref t, ref attr) => write!(f, "{}.{}", t, attr),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Arr(Box<Type>, Box<Type>),
    Record(AssocList<String, Box<Type>>),
    Var(String),
    All(String, Box<Type>),
    // The second parameter is a general Type in the TAPL implementation but
    // currently it's only possible to instantiate a Type::Some with a Record type
    // anyways so use an AssocList directly
    Some(String, AssocList<String, Box<Type>>),
}

pub trait Resolvable {
    fn resolve(&self, ctx: &TypeContext) -> Result<Self, String>
    where
        Self: marker::Sized;
}

impl Resolvable for Type {
    fn resolve(&self, ctx: &TypeContext) -> Result<Type, String> {
        match *self {
            Type::Bool => Ok(Type::Bool),
            Type::Int => Ok(Type::Int),
            Type::Arr(ref from, ref to) => Ok(Type::Arr(
                Box::new(from.resolve(ctx)?),
                Box::new(to.resolve(ctx)?),
            )),
            Type::Record(ref fields) => Ok(Type::Record(fields.resolve(ctx)?)),
            Type::Var(ref s) => ctx.lookup(s).ok_or(s.clone()),
            Type::All(ref s, ref ty) => {
                Ok(Type::All(s.clone(), Box::new(ty.resolve(ctx)?)))
            }
            // TODO: do we need to shadow s?
            Type::Some(ref s, ref sigs) => {
                Ok(Type::Some(s.clone(), sigs.resolve(ctx)?))
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            // TODO: parenthesize
            Type::Arr(ref from, ref to) => write!(f, "({} -> {})", from, to),
            // TODO: refactor repeated code?
            Type::Record(ref rec) => write!(f, "{{{}}}", rec),
            Type::Var(ref s) => write!(f, "{}", s),
            Type::All(ref s, ref ty) => write!(f, "∀{}. {}", s, ty),
            Type::Some(ref s, ref sigs) => write!(f, "∃{}. {}", s, sigs),
        }
    }
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

impl ArithOp {
    pub fn return_type(&self) -> Type {
        use self::ArithOp::*;
        match *self {
            Mul | Div | Add | Sub | Mod => Type::Int,
            Eq_ | Neq | Gt | Lt | Gte | Lte => Type::Bool,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BoolOp {
    And,
    Or,
}

impl fmt::Display for BoolOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BoolOp::And => write!(f, "and"),
            BoolOp::Or => write!(f, "or"),
        }
    }
}
