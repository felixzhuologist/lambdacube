use assoclist::{AssocList, TypeContext};

use std::fmt;

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
    Unit,
    Bool(bool),
    Not(Box<Term>),
    Var(String),
    Int(i32),
    Abs(String, Box<Type>, Box<Term>),
    InfAbs(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    Arith(Box<Term>, ArithOp, Box<Term>),
    Logic(Box<Term>, BoolOp, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(String, Box<Term>, Box<Term>),
    Record(AssocList<String, Box<Term>>),
    Proj(Box<Term>, String),
    SetProj(Box<Term>, String, Box<Term>),
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
            Term::Unit
            | Term::Int(_)
            | Term::Bool(_)
            | Term::Abs(_, _, _)
            | Term::InfAbs(_, _) => true,
            Term::Not(box t) => t.is_val(),
            Term::Record(fields) => {
                fields.inner.iter().all(|(_, val)| val.is_val())
            }
            Term::App(_, _)
            | Term::Var(_)
            | Term::Arith(_, _, _)
            | Term::Logic(_, _, _)
            | Term::If(_, _, _)
            | Term::Let(_, _, _)
            | Term::SetProj(_, _, _)
            | Term::Proj(_, _) => false,
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Unit => write!(f, "()"),
            Term::Bool(b) => write!(f, "{}", b),
            Term::Not(ref t) => write!(f, "not {}", t),
            Term::Var(ref s) => write!(f, "{}", s),
            Term::Int(n) => write!(f, "{}", n),
            Term::Abs(_, _, _) | Term::InfAbs(_, _) => write!(f, "<fun>"),
            Term::App(ref func, ref arg) => write!(f, "{} {}", func, arg),
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
            Term::SetProj(ref t, ref attr, ref val) => {
                write!(f, "{}.{} <- {}", t, attr, val)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Unit,
    Arr(Box<Type>, Box<Type>),
    Record(AssocList<String, Box<Type>>),
    Var(String),
}

impl Type {
    pub fn resolve(&self, ctx: &TypeContext) -> Result<Type, String> {
        match *self {
            Type::Unit => Ok(Type::Unit),
            Type::Bool => Ok(Type::Bool),
            Type::Int => Ok(Type::Int),
            Type::Arr(ref from, ref to) => Ok(Type::Arr(
                Box::new(from.resolve(ctx)?),
                Box::new(to.resolve(ctx)?),
            )),
            Type::Record(ref fields) => {
                let mut new_fields = Vec::new();
                for (key, box val) in fields.inner.iter() {
                    new_fields.push((key.clone(), Box::new(val.resolve(ctx)?)))
                }
                Ok(Type::Record(AssocList::from_vec(new_fields)))
            }
            Type::Var(ref s) => ctx.lookup(s).ok_or(s.clone()),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            // TODO: parenthesize
            Type::Arr(ref from, ref to) => write!(f, "({} -> {})", from, to),
            // TODO: refactor repeated code?
            Type::Record(ref rec) => write!(
                f,
                "{{{}}}",
                rec.inner
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Var(ref s) => write!(f, "{}", s),
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
