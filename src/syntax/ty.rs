use assoclist::{AssocList, TypeContext};
use syntax::{Kind, Substitutable};

use std::fmt;
use std::marker;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Arr(Box<Type>, Box<Type>),
    Record(AssocList<String, Box<Type>>),
    Var(String),
    // TODO: is this extra variant necessary?
    BoundedVar(String, Box<Type>),
    All(String, Box<Type>),
    // This could encompass the All type by putting Top as the second type argument
    // but with the way Terms and Types are shared across all typecheckers it
    // is probably simpler to keep them separate for now
    BoundedAll(String, Box<Type>, Box<Type>),
    // The second parameter is a general Type in the TAPL implementation but
    // currently it's only possible to instantiate a Type::Some with a Record type
    // anyways so use an AssocList directly
    Some(String, AssocList<String, Box<Type>>),
    TyAbs(String, Kind, Box<Type>),
    TyApp(Box<Type>, Box<Type>),
}

impl Type {
    /// Return the least nonvariable supertype of self
    pub fn expose(&self, ctx: &TypeContext) -> Result<Type, String> {
        if let Type::Var(ref s) = self {
            match ctx.lookup(s).ok_or(s.clone())? {
                Type::Var(ref s2) if s == s2 => Ok(self.clone()),
                ty => ty.expose(ctx),
            }
        } else {
            Ok(self.clone())
        }
    }
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
            t @ Bool | t @ Int | t @ BoundedVar(_, _) => Ok(t.clone()),
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
            TyAbs(ref arg, kind, box body) => Ok(TyAbs(
                arg.clone(),
                kind.clone(),
                // TODO: we don't expect arg to be defined here...
                Box::new(body.resolve(ctx)?),
            )),
            TyApp(box func, box arg) => Ok(TyApp(
                Box::new(func.resolve(ctx)?),
                Box::new(arg.resolve(ctx)?),
            )),
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
            Type::BoundedVar(ref s, _) | Type::Var(ref s) => write!(f, "{}", s),
            Type::All(ref s, ref ty) => write!(f, "∀{}. {}", s, ty),
            Type::BoundedAll(ref s, ref ty, ref bound) => {
                write!(f, "∀{} <: {}. {}", s, bound, ty)
            }
            Type::Some(ref s, ref sigs) => write!(f, "∃{}. {}", s, sigs),
            Type::TyAbs(_, _, _) => write!(f, "<tyfun>"),
            Type::TyApp(ref func, ref arg) => write!(f, "{} {}", func, arg),
        }
    }
}

impl Substitutable<Type> for Type {
    fn applysubst(self, varname: &str, var: &Type) -> Type {
        use self::Type::*;
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
            BoundedVar(s, bound) => if s == varname {
                var.clone()
            } else {
                BoundedVar(s, bound)
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
            TyAbs(param, kind, box body) => if param != varname {
                TyAbs(param, kind, Box::new(body))
            } else {
                TyAbs(param, kind, Box::new(body.applysubst(varname, var)))
            },
            TyApp(box func, box val) => TyApp(
                Box::new(func.applysubst(varname, var)),
                Box::new(val.applysubst(varname, var)),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
