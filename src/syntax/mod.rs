pub mod kind;
pub mod term;
pub mod ty;

pub use self::kind::Kind;
pub use self::term::{ArithOp, BoolOp, Term};
pub use self::ty::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Command {
    Binder(Binder),
    Term(Box<Term>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Binder {
    VarBind(String, Term),
    TyBind(String, Type),
    ModuleBind(String, String, Term),
}

pub trait Substitutable<T: Clone> {
    /// Substitute a variable into Self, respecting local scopes
    fn applysubst(self, varname: &str, var: &T) -> Self;
}
