use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Var(String),
    Int(i32),
    Abs(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    // when performing a substitution, we wrap the body in a Return to let any
    // AST manipulators know when to pop from the context
    Return(Box<Term>)
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
            _ => false,
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Term::Var(ref s) => write!(f, "{}", s),
            Term::Int(n) => write!(f, "{}", n),
            Term::Abs(ref argname, ref body) =>
                write!(f, "fun {} . {}", argname, body),
            Term::App(ref func, ref arg) =>
                write!(f, "{} {}", func, arg),
            Term::Return(ref term) => write!(f, "{}", term)
        }
    }
}