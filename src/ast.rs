use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Bool(bool),
    Not(Box<Term>),
    Var(String),
    Int(i32),
    Abs(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    // when performing a substitution, we wrap the body in a Return to let any
    // AST manipulators know when to pop from the context
    Return(Box<Term>),
    Arith(Box<Term>, ArithOp, Box<Term>),
    Logic(Box<Term>, BoolOp, Box<Term>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArithOp {
    Mul,
    Div,
    Add,
    Sub
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
            Term::Abs(ref argname, ref body) =>
                write!(f, "fun {} . {}", argname, body),
            Term::App(ref func, ref arg) =>
                write!(f, "{} {}", func, arg),
            Term::Return(ref term) => write!(f, "{}", term),
            Term::Arith(ref l, ref op, ref r) => write!(f, "{} {} {}", l, op, r),
            Term::Logic(ref l, ref op, ref r) => write!(f, "{} {} {}", l, op, r),
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

