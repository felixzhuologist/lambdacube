use assoclist::TypeContext as Context;
use errors::TypeError;
use eval::Eval;
use std::marker;
use syntax::{Term, Type};

export_kindless_typechecker!(typecheck);

pub fn typecheck(
    term: &Term,
    context: &mut Context,
) -> Result<Type, TypeError> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::Int(_) => Ok(Type::Int),
        Term::Not(box t) => match typecheck(t, context)? {
            Type::Bool => Ok(Type::Bool),
            _ => Err(TypeError::NegateNonBool),
        },
        Term::Var(s) => {
            context.lookup(s).ok_or(TypeError::NameError(s.to_string()))
        }
        Term::Abs(param, type_, box body) => {
            let ty = type_.resolve(context)?;
            context.push(param.clone(), ty.clone());
            let result = Ok(Type::Arr(
                Box::new(ty),
                Box::new(typecheck(body, context)?),
            ));
            context.pop();
            result
        }
        Term::App(box func, box val) => match typecheck(func, context)? {
            Type::Arr(box in_type, box out_type) => {
                match typecheck(val, context)? {
                    ref t if *t == in_type => Ok(out_type.clone()),
                    t => Err(TypeError::ArgMismatch(in_type, t)),
                }
            }
            _ => Err(TypeError::FuncApp),
        },
        Term::Arith(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Int, Type::Int) => Ok(op.return_type()),
                (l, r) => Err(TypeError::Arith(*op, l, r)),
            }
        }
        Term::Logic(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Bool, Type::Bool) => Ok(Type::Bool),
                (l, r) => Err(TypeError::Logic(*op, l, r)),
            }
        }
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
        }
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, context)?;
            context.push(varname.clone(), val_type);
            let result = Ok(typecheck(term, context)?);
            context.pop();
            result
        }
        Term::Record(fields) => {
            Ok(Type::Record(fields.map_typecheck(typecheck, context)?))
        }
        Term::Proj(box term, key) => match typecheck(term, context)? {
            Type::Record(fields) => fields
                .lookup(&key)
                .ok_or(TypeError::InvalidKey(key.clone())),
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::TyAbs(_, _)
        | Term::TyApp(_, _)
        | Term::InfAbs(_, _)
        | Term::Pack(_, _, _)
        | Term::Unpack(_, _, _, _)
        | Term::KindedTyAbs(_, _, _)
        | Term::BoundedTyAbs(_, _, _)
        | Term::QBool(_)
        | Term::QInt(_)
        | Term::QAbs(_, _, _)
        | Term::QRec(_) => Err(TypeError::Unsupported),
    }
}

pub trait Resolve {
    fn resolve(&self, ctx: &mut Context) -> Result<Self, TypeError>
    where
        Self: marker::Sized;
}

impl Resolve for Type {
    /// wrapper around ty.eval() that errors if it contains a type operator, for
    /// use by typecheckers that don't support type operators
    fn resolve(&self, ctx: &mut Context) -> Result<Self, TypeError> {
        if has_ty_operators(self) {
            Err(TypeError::Unsupported)
        } else {
            self.eval(ctx)
        }
    }
}

pub fn has_ty_operators(ty: &Type) -> bool {
    match ty {
        Type::Bool
        | Type::QBool
        | Type::Int
        | Type::QInt
        | Type::Top
        | Type::Var(_)
        | Type::BoundedVar(_, _) => false,
        Type::TyAbs(_, _, _) | Type::TyApp(_, _) => true,
        Type::Record(fields) | Type::Some(_, fields) | Type::QRec(fields) => {
            fields
                .inner
                .iter()
                .any(|(_, ref val)| has_ty_operators(val))
        }
        Type::All(_, ref fun) | Type::KindedAll(_, ref fun, _) => {
            has_ty_operators(fun)
        }
        Type::BoundedAll(_, ref l, ref r)
        | Type::Arr(ref l, ref r)
        | Type::QArr(ref l, ref r) => {
            has_ty_operators(l) || has_ty_operators(r)
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use assoclist::TypeContext;
    use grammar;

    pub fn typecheck_code(code: &str) -> String {
        typecheck(
            &grammar::TermParser::new().parse(code).unwrap(),
            &mut TypeContext::empty(),
        ).map(|ty| ty.to_string())
        .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn e2e_type() {
        assert_eq!(typecheck_code("if true then 0 else 2"), "Int");
        assert_eq!(
            typecheck_code("let x = 0 in if x + 2 then 0 else 2"),
            "If/else condition must be a Bool"
        );
        assert_eq!(typecheck_code("fun (x: Int) -> x >= 0"), "(Int -> Bool)");
        assert_eq!(typecheck_code("3 4"), "Tried to apply non function type");
        assert_eq!(typecheck_code("(fun (x: Int) -> x*2 + 1) 0"), "Int");

        let tyfun = "tyfun (X: *) => X -> X";
        assert_eq!(
            typecheck_code(&format!("fun (x: {}) -> x", tyfun)),
            "Attempted to use an unsupported feature for the current type system");
    }
}
