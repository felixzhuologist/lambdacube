//! Universal Types

use assoclist::{AssocList, TypeContext as Context};
use errors::TypeError;
use syntax::{Resolvable, Substitutable, Term, Type};

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
        Term::Abs(param, box type_, box body) => {
            let ty = type_
                .resolve(context)
                .map_err(|s| TypeError::NameError(s))?;
            context.push(param.clone(), ty.clone());
            let result = Ok(Type::Arr(
                Box::new(ty),
                Box::new(typecheck(body, context)?),
            ));
            context.pop();
            result
        }
        Term::TyAbs(param, box body) => {
            context.push(param.clone(), Type::Var(param.clone()));
            let result = Ok(Type::All(
                param.clone(),
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
        Term::TyApp(box func, box ty) => match typecheck(func, context)? {
            Type::All(s, box body) => Ok(body.clone().applysubst(&s, ty)),
            _ => Err(TypeError::TyFuncApp),
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
            let result = typecheck(term, context)?;
            context.pop();
            Ok(result)
        }
        Term::Record(fields) => {
            let mut types = Vec::new();
            for (key, box val) in fields.inner.iter() {
                types.push((key.clone(), Box::new(typecheck(val, context)?)))
            }
            Ok(Type::Record(AssocList::from_vec(types)))
        }
        Term::Proj(box term, key) => match typecheck(term, context)? {
            Type::Record(fields) => match fields.lookup(&key) {
                Some(type_) => Ok(*type_),
                None => Err(TypeError::InvalidKey(key.clone())),
            },
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::Pack(box witness, impls, box ty) => {
            let ty =
                ty.resolve(context).map_err(|s| TypeError::NameError(s))?;
            if let Type::Some(name, sigs) = ty {
                let mut expected = sigs.clone().applysubst(&name, witness).inner;

                // TODO: code reuse
                let mut actual = Vec::new();
                for (name, box val) in impls.inner.iter() {
                    actual.push((
                        name.clone(),
                        Box::new(typecheck(val, context)?),
                    ))
                }

                expected.sort_by_key(|(s, _)| s.clone());
                actual.sort_by_key(|(s, _)| s.clone());
                if actual == expected {
                    Ok(Type::Some(name.clone(), sigs))
                } else {
                    Err(TypeError::ModuleMismatch(
                        AssocList::from_vec(expected),
                        AssocList::from_vec(actual),
                    ))
                }
            } else {
                Err(TypeError::ExpectedSome)
            }
        }
        Term::Unpack(tyvar, var, box mod_, box term) => {
            if let Type::Some(_, sigs) = typecheck(mod_, context)? {
                context.push(tyvar.clone(), Type::Var(tyvar.clone()));
                context.push(var.clone(), Type::Record(sigs.clone()));
                let result = typecheck(term, context)?;
                context.pop();
                context.pop();
                Ok(result)
            } else {
                Err(TypeError::ExpectedSome)
            }
        }
        Term::InfAbs(_, _) => Err(TypeError::Unsupported),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::TypeContext as Context;
    use grammar;

    pub fn typecheck_code(code: &str) -> String {
        typecheck(
            &grammar::TermParser::new().parse(code).unwrap(),
            &mut Context::empty(),
        ).map(|ty| ty.to_string())
        .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn e2e_univ() {
        assert_eq!(typecheck_code("fun[X] (x: X) -> x"), "∀X. (X -> X)");
        assert_eq!(
            typecheck_code("fun[X, Y] (f: X -> Y) (x: X) -> f x"),
            "∀X. ∀Y. ((X -> Y) -> (X -> Y))"
        );
        assert_eq!(
            typecheck_code("let f = fun[X] (x: X) -> x in f[Int]"),
            "(Int -> Int)"
        );
        assert_eq!(
            typecheck_code("let f = fun[X] (x: X) -> x in f[Int] 0"),
            "Int"
        );
    }
}
