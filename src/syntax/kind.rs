use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Star,
    Arr(Box<Kind>, Box<Kind>),
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Kind::Star => write!(f, "*"),
            // TODO: parenthesize
            Kind::Arr(ref l, ref r) => write!(f, "({} -> {})", l, r),
        }
    }
}
