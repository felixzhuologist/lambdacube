use std::fmt;

use assoclist::{AssocList, TypeContext};
use syntax::{Kind, Substitutable};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Top,
    Bool,
    Int,
    Arr(Box<Type>, Box<Type>),
    Record(AssocList<String, Type>),
    Var(String),
    BoundedVar(String, Box<Type>),
    All(String, Box<Type>, Box<Type>),
    // We only allow the value component of an existential to be a record
    Some(String, AssocList<String, Type>),
    TyAbs(String, Kind, Box<Type>),
    TyApp(Box<Type>, Box<Type>),
    QBool,
    QInt,
    QArr(Box<Type>, Box<Type>),
    QRec(AssocList<String, Type>),
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

    pub fn is_qualified(&self) -> bool {
        use self::Type::*;
        match self {
            QBool | QInt | QArr(_, _) | QRec(_) => true,
            Top | Bool | Int | Var(_) | BoundedVar(_, _) => false,
            Arr(ref l, ref r) => l.is_qualified() || r.is_qualified(),
            Record(fields) => {
                fields.inner.iter().any(|(_, v)| v.is_qualified())
            }
            _ => unimplemented!(),
        }
    }

    pub fn is_linear_val(&self) -> bool {
        use self::Type::*;
        match self {
            QBool | QInt | QRec(_) => true, // QArr has different type rules
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Top => write!(f, "Top"),
            Type::Bool => write!(f, "Bool"),
            Type::QBool => write!(f, "lin Bool"),
            Type::Int => write!(f, "Int"),
            Type::QInt => write!(f, "lin Int"),
            // TODO: parenthesize
            Type::Arr(ref from, ref to) => write!(f, "({} -> {})", from, to),
            Type::QArr(ref from, ref to) => {
                write!(f, "lin ({} -> {})", from, to)
            }
            Type::Record(ref rec) => write!(f, "{{{}}}", rec),
            Type::QRec(ref rec) => write!(f, "lin {{{}}}", rec),
            Type::BoundedVar(ref s, _) | Type::Var(ref s) => write!(f, "{}", s),
            Type::All(ref s, ref ty, ref bound) => match **bound {
                Type::Top => write!(f, "∀{}. {}", s, ty),
                _ => write!(f, "∀{} <: {}. {}", s, bound, ty),
            },
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
            t @ Bool | t @ Int | t @ Top | t @ QBool | t @ QInt => t,
            Arr(box from, box to) => Arr(
                Box::new(from.applysubst(varname, var)),
                Box::new(to.applysubst(varname, var)),
            ),
            QArr(box from, box to) => QArr(
                Box::new(from.applysubst(varname, var)),
                Box::new(to.applysubst(varname, var)),
            ),
            Record(fields) => Record(fields.applysubst(varname, var)),
            QRec(fields) => QRec(fields.applysubst(varname, var)),
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
            All(param, box body, box bound) => if param != varname {
                All(
                    param,
                    Box::new(body.applysubst(varname, var)),
                    Box::new(bound),
                )
            } else {
                All(param, Box::new(body), Box::new(bound))
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
            Box::new(Type::Top),
        );
        assert_eq!(func.clone().applysubst(&varname, &var), func);
    }
}
