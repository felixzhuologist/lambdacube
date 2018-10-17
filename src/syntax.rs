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
    ModuleBind(String, String, Term),
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
    /// bounded type abstraction
    BoundedTyAbs(String, Box<Term>, Box<Type>),
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
    /// introduce an existential: provide the witness type, the implementation,
    /// and the existential type it should inhabit - the term must have its
    /// type explicitly annotated when being defined
    // Analogously to Type::Some, this could take a general Term
    // but right now we assume that it will be a Term::Record anyways
    Pack(Box<Type>, AssocList<String, Box<Term>>, Box<Type>),
    /// unpack an existential and give it a name for the scope of the third term
    Unpack(String, String, Box<Term>, Box<Term>),
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
            | Term::BoundedTyAbs(_, _, _)
            | Term::InfAbs(_, _) => true,
            Term::Not(box t) => t.is_val(),
            Term::Pack(_, fields, _) | Term::Record(fields) => {
                fields.inner.iter().all(|(_, val)| val.is_val())
            }
            Term::App(_, _)
            | Term::TyApp(_, _)
            | Term::Var(_)
            | Term::Arith(_, _, _)
            | Term::Logic(_, _, _)
            | Term::If(_, _, _)
            | Term::Let(_, _, _)
            | Term::Proj(_, _)
            | Term::Unpack(_, _, _, _) => false,
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
            Term::Abs(_, _, _)
            | Term::InfAbs(_, _)
            | Term::TyAbs(_, _)
            | Term::BoundedTyAbs(_, _, _) => write!(f, "<fun>"),
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
            Term::Pack(_, _, _) => write!(f, "<mod>"), // TODO
            Term::Unpack(ref tyname, ref tname, ref mod_, ref term) => {
                write!(f, "open {} as {}: {} in {}", mod_, tname, tyname, term)
            }
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
    // This could encompass the All type by putting Top as the second type argument
    // but with the way Terms and Types are shared across all typecheckers it
    // is probably simpler to keep them separate for now
    BoundedAll(String, Box<Type>, Box<Type>),
    // The second parameter is a general Type in the TAPL implementation but
    // currently it's only possible to instantiate a Type::Some with a Record type
    // anyways so use an AssocList directly
    Some(String, AssocList<String, Box<Type>>),
}

pub trait Resolvable {
    /// Tries to resolve all term/type variables inside of itself, erroring if any
    /// are undefined
    fn resolve(&self, ctx: &TypeContext) -> Result<Self, String>
    where
        Self: marker::Sized;
}

impl Resolvable for Type {
    fn resolve(&self, ctx: &TypeContext) -> Result<Type, String> {
        use self::Type::*;
        match self {
            t @ Bool | t @ Int => Ok(t.clone()),
            Arr(ref from, ref to) => Ok(Arr(
                Box::new(from.resolve(ctx)?),
                Box::new(to.resolve(ctx)?),
            )),
            Record(ref fields) => Ok(Record(fields.resolve(ctx)?)),
            Var(ref s) => ctx.lookup(s).ok_or(s.clone()),
            All(ref s, ref ty) => {
                Ok(All(s.clone(), Box::new(ty.resolve(ctx)?)))
            }
            BoundedAll(ref s, ref ty, ref bound) => Ok(BoundedAll(
                s.clone(),
                Box::new(ty.resolve(ctx)?),
                Box::new(bound.resolve(ctx)?),
            )),
            Some(ref s, ref sigs) => Ok(Some(s.clone(), sigs.resolve(ctx)?)),
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
            Type::Record(ref rec) => write!(f, "{{{}}}", rec),
            Type::Var(ref s) => write!(f, "{}", s),
            Type::All(ref s, ref ty) => write!(f, "∀{}. {}", s, ty),
            Type::BoundedAll(ref s, ref ty, ref bound) => {
                write!(f, "∀{}<:{}. {}", s, bound, ty)
            }
            Type::Some(ref s, ref sigs) => write!(f, "∃{}. {}", s, sigs),
        }
    }
}

pub trait Substitutable<T: Clone> {
    /// Substitute a variable into Self, respecting local scopes
    fn applysubst(self, varname: &str, var: &T) -> Self;
}

impl Substitutable<Term> for Term {
    fn applysubst(self, varname: &str, var: &Term) -> Term {
        use self::Term::*;
        match self {
            t @ Bool(_) | t @ Int(_) => t,
            Not(box t) => Not(Box::new(t.applysubst(varname, var))),
            Var(s) => if s == varname {
                var.clone()
            } else {
                Var(s)
            },
            Abs(param, ty, box body) => if param != varname {
                Abs(param, ty, Box::new(body.applysubst(varname, var)))
            } else {
                Abs(param, ty, Box::new(body))
            },
            InfAbs(param, box body) => if param != varname {
                InfAbs(param, Box::new(body.applysubst(varname, var)))
            } else {
                InfAbs(param, Box::new(body))
            },
            TyAbs(param, box body) => {
                let body = body.applysubst(varname, var);
                TyAbs(param, Box::new(body))
            }
            BoundedTyAbs(param, box body, ty) => {
                let body = body.applysubst(varname, var);
                BoundedTyAbs(param, Box::new(body), ty)
            }
            App(box func, box val) => App(
                Box::new(func.applysubst(varname, var)),
                Box::new(val.applysubst(varname, var)),
            ),
            TyApp(box func, val) => {
                TyApp(Box::new(func.applysubst(varname, var)), val)
            }
            Arith(box l, op, box r) => Arith(
                Box::new(l.applysubst(varname, var)),
                op,
                Box::new(r.applysubst(varname, var)),
            ),
            Logic(box l, op, box r) => Logic(
                Box::new(l.applysubst(varname, var)),
                op,
                Box::new(r.applysubst(varname, var)),
            ),
            If(box cond, box if_, box else_) => If(
                Box::new(cond.applysubst(varname, var)),
                Box::new(if_.applysubst(varname, var)),
                Box::new(else_.applysubst(varname, var)),
            ),
            Let(s, val, box rest) => if s != varname {
                Let(s, val, Box::new(rest.applysubst(varname, var)))
            } else {
                Let(s, val, Box::new(rest))
            },
            Record(fields) => Record(fields.applysubst(varname, var)),
            Proj(box t, field) => {
                Proj(Box::new(t.applysubst(varname, var)), field)
            }
            Pack(witness, impls, ty) => {
                Pack(witness, impls.applysubst(varname, var), ty)
            }
            Unpack(ty, val, box mod_, box term) => Unpack(
                ty,
                val,
                Box::new(mod_.applysubst(varname, var)),
                Box::new(term.applysubst(varname, var)),
            ),
        }
    }
}

impl Substitutable<Type> for Type {
    fn applysubst(self, varname: &str, var: &Type) -> Type {
        use self::Type::{All, Arr, Bool, BoundedAll, Int, Record, Var};
        match self {
            t @ Bool | t @ Int => t,
            Arr(box from, box to) => Arr(
                Box::new(from.applysubst(varname, var)),
                Box::new(to.applysubst(varname, var)),
            ),
            Record(fields) => Record(fields.applysubst(varname, var)),
            Var(s) => if s == varname {
                var.clone()
            } else {
                Var(s)
            },
            All(param, box body) => if param != varname {
                All(param, Box::new(body.applysubst(varname, var)))
            } else {
                All(param, Box::new(body))
            },
            BoundedAll(param, box body, box bound) => if param != varname {
                BoundedAll(
                    param,
                    Box::new(body.applysubst(varname, var)),
                    Box::new(bound),
                )
            } else {
                BoundedAll(param, Box::new(body), Box::new(bound))
            },
            Type::Some(param, sigs) => if param != varname {
                Type::Some(param, sigs.applysubst(varname, var))
            } else {
                Type::Some(param, sigs)
            },
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn term_subst() {
        let varname = String::from("x");
        let var = Term::Int(5);
        assert_eq!(
            Term::Var(String::from("x")).applysubst(&varname, &var),
            Term::Int(5)
        );
        let id_func = Term::InfAbs(
            String::from("x"),
            Box::new(Term::Var(String::from("x"))),
        );
        assert_eq!(id_func.clone().applysubst(&varname, &var), id_func);
    }

    #[test]
    fn ty_subst() {
        let varname = String::from("X");
        let var = Type::Int;
        assert_eq!(
            Type::Var(String::from("X")).applysubst(&varname, &var),
            Type::Int
        );
        let func = Type::All(
            String::from("X"),
            Box::new(Type::Var(String::from("X"))),
        );
        assert_eq!(func.clone().applysubst(&varname, &var), func);
    }
}
