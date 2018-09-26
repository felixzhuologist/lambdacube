use assoclist::AssocList;
use ast::Term;
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

pub type Context = AssocList<String, Type>;

pub fn typecheck(term: &Term, context: &mut Context) -> Result<Type, &'static str> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::Int(_) => Ok(Type::Int),
        Term::Not(box t) => {
            if typecheck(t, context)? == Type::Bool {
                Ok(Type::Bool)
            } else {
                Err("Cannot negate a non boolean value")
            }
        },
        Term::Var(s) => {
            match context.lookup(s) {
                Some(type_) => Ok(type_),
                None => Err("Name not defined: {}")
            }
        },
        Term::Abs(param, box type_, box body) => {
            // TODO: when do these get popped from the context?
            context.push(param.clone(), type_.clone());
            Ok(Type::Arr(
                Box::new(type_.clone()),
                Box::new(typecheck(body,context)?)))
        },
        Term::App(box func, box val) => {
            match typecheck(func, context)? {
                Type::Arr(box in_type, box out_type) => {
                    if typecheck(val, context)? == in_type {
                        Ok(out_type.clone())
                    } else {
                        Err("Function argument does not match input type")
                    }
                },
                _ => Err("Applying non function value")
            }
        },
        Term::Arith(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Int, Type::Int) => match op {
                    Mul | Div | Add | Sub | Mod => Ok(Type::Int),
                    Eq_ | Neq | Gt | Lt | Gte | Lte => Ok(Type::Bool),
                },
                _ => Err("Arithmetic operators can only be used for ints")
            }
        },
        Term::Logic(box left, _op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Bool, Type::Bool) => Ok(Type::Bool),
                _ => Err("Arithmetic operators can only be used for ints")
            }
        },
        Term::If(box cond, box if_, box else_) => {
            let result = typecheck(if_, context)?;
            if typecheck(cond, context)? != Type::Bool {
                Err("If/else condition is not a boolean")
            } else if result != typecheck(else_, context)? {
                Err("If/else branch arms don't match")
            } else {
                Ok(result)
            }
        },
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, context)?;
            context.push(varname.clone(), val_type);
            Ok(typecheck(term, context)?)
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
                    None => Err("Record does not have key"),
                },
                _ => Err("Can only project record types")
            }
        },
        _ => unimplemented!()
    }
}
