use assoclist::AssocList;
use ast::Term;
use ast::{ArithOp, BoolOp};
use ast::ArithOp::*;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Arr(Box<Type>, Box<Type>),
    Record(AssocList<String, Box<Type>>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Bool => write!(f, "Bool"),
            Type::Int => write!(f, "Int"),
            // TODO: parenthesize
            Type::Arr(ref from, ref to) => write!(f, "{} -> {}", from, to),
            // TODO: refactor repeated code?
            Type::Record(ref rec) => write!(f, "{{{}}}", rec.inner.iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<String>>()
                .join(", ")),
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    NegateNonBool,
    NameError(String),
    ArgMismatch(Type, Type),
    FuncApp,
    Arith(ArithOp, Type, Type),
    Logic(BoolOp, Type, Type),
    IfElseCond,
    IfElseArms(Type, Type),
    InvalidKey(String),
    ProjectNonRecord
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeError::NegateNonBool =>
                write!(f, "Attempted to negate non boolean value"),
            TypeError::NameError(ref s) =>
                write!(f, "Variable {} not found", s),
            TypeError::ArgMismatch(ref expected, ref actual) =>
                write!(f, "Expected argument of type {} got but {} instead",
                    expected, actual),
            TypeError::FuncApp => write!(f, "Tried to apply non function type"),
            TypeError::Arith(ref op, ref left, ref right) =>
                write!(f, "Cannot apply {} to {} and {}", op, left, right),
            TypeError::Logic(ref op, ref left, ref right) =>
                write!(f, "Cannot apply {} to {} and {}", op, left, right),
            TypeError::IfElseCond => write!(f, "If/else condition must be a Bool"),
            TypeError::IfElseArms(ref left, ref right) =>
                write!(f, "If/else arms have mismatched types: {} and {}",
                    left, right),
            TypeError::InvalidKey(ref s) =>
                write!(f, "Record does not have key {}", s),
            TypeError::ProjectNonRecord => write!(f, "Can only project record types")
        }
    }
}

pub type Context = AssocList<String, Type>;

pub fn typecheck(term: &Term, context: &mut Context) -> Result<Type, TypeError> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::Int(_) => Ok(Type::Int),
        Term::Not(box t) => {
            match typecheck(t, context)? {
                Type::Bool => Ok(Type::Bool),
                _ => Err(TypeError::NegateNonBool)
            }
        },
        Term::Var(s) => {
            match context.lookup(s) {
                Some(type_) => Ok(type_),
                None => Err(TypeError::NameError(s.to_string()))
            }
        },
        Term::Abs(param, box type_, box body) => {
            context.push(param.clone(), type_.clone());
            let result = Ok(Type::Arr(
                Box::new(type_.clone()),
                Box::new(typecheck(body,context)?)));
            context.pop();
            result
        },
        Term::App(box func, box val) => {
            match typecheck(func, context)? {
                Type::Arr(box in_type, box out_type) => {
                    match typecheck(val, context)? {
                        ref t if is_subtype(t, &in_type) => Ok(out_type.clone()),
                        t => Err(TypeError::ArgMismatch(in_type, t))
                    }
                },
                _ => Err(TypeError::FuncApp)
            }
        },
        Term::Arith(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Int, Type::Int) => match op {
                    Mul | Div | Add | Sub | Mod => Ok(Type::Int),
                    Eq_ | Neq | Gt | Lt | Gte | Lte => Ok(Type::Bool),
                },
                (l, r) => Err(TypeError::Arith(*op, l, r))
            }
        },
        Term::Logic(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Bool, Type::Bool) => Ok(Type::Bool),
                (l, r) => Err(TypeError::Logic(*op, l, r))
            }
        },
        Term::If(box cond, box if_, box else_) => {
            let left = typecheck(if_, context)?;
            let right = typecheck(else_, context)?;
            if typecheck(cond, context)? != Type::Bool {
                Err(TypeError::IfElseCond)
            } else if left != right {
                Err(TypeError::IfElseArms(left, right))
            } else {
                Ok(left)
            }
        },
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, context)?;
            context.push(varname.clone(), val_type);
            let result = Ok(typecheck(term, context)?);
            context.pop();
            result
        },
        Term::Record(fields) => {
            let mut types = Vec::new();
            for (key, box val) in fields.inner.iter() {
                types.push((key.clone(), Box::new(typecheck(val, context)?)))
            }
            Ok(Type::Record(AssocList::from_vec(types)))
        },
        Term::Proj(box term, key) => {
            match typecheck(term, context)? {
                Type::Record(fields) => match fields.lookup(&key) {
                    Some(type_) => Ok(*type_),
                    None => Err(TypeError::InvalidKey(key.clone())),
                },
                _ => Err(TypeError::ProjectNonRecord)
            }
        },
        _ => unimplemented!()
    }
}

pub fn is_subtype(left: &Type, right: &Type) -> bool {
   left == right ||
   match (left, right) {
        (Type::Record(fields1), Type::Record(fields2)) => {
            fields2.inner.iter().all(|(key, ltype)| match fields1.lookup(key) {
                Some(ref rtype) => is_subtype(ltype, rtype),
                None => false
            })

        },
        (Type::Arr(in1, out1), Type::Arr(in2, out2)) => {
            is_subtype(in2, in1) && is_subtype(out1, out2)
        },
        _ => false
   }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn subtyping() {
        // types are equal
        assert!(is_subtype(&Type::Bool, &Type::Bool));
        assert!(is_subtype(
            &Type::Arr(Box::new(Type::Int), Box::new(Type::Bool)),
            &Type::Arr(Box::new(Type::Int), Box::new(Type::Bool))));

        // width subtyping
        let small_rec = Type::Record(AssocList::from_vec(
            vec![("a".to_string(), Box::new(Type::Int))]));
        let big_rec = Type::Record(AssocList::from_vec(
            vec![("a".to_string(), Box::new(Type::Int)), ("b".to_string(), Box::new(Type::Bool))]));
        assert!(is_subtype(&big_rec, &small_rec));

        // depth subtyping
        let shallow = Type::Record(AssocList::from_vec(
            vec![("a".to_string(), Box::new(small_rec.clone()))]));
        let deep = Type::Record(AssocList::from_vec(
            vec![("a".to_string(), Box::new(big_rec.clone()))]));
        assert!(is_subtype(&shallow, &deep));

        // function subtyping is contravariant for input, covariant for output
        let f1 = Type::Arr(
            Box::new(small_rec.clone()),
            Box::new(shallow.clone()));
        let f2 = Type::Arr(
            Box::new(big_rec.clone()),
            Box::new(deep.clone()));
        assert!(is_subtype(&f1, &f2));
    }
}
