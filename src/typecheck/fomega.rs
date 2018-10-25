// TODO: kinded existentials

use assoclist::{KindContext, TypeContext};
use errors::TypeError;
use kindcheck::kindcheck;
use syntax::{Kind, Substitutable, Term, Type};
use typecheck::omega::Simplify;

pub fn typecheck(
    term: &Term,
    type_ctx: &mut TypeContext,
    kind_ctx: &mut KindContext,
) -> Result<Type, TypeError> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::Int(_) => Ok(Type::Int),
        Term::Not(box t) => match typecheck(t, type_ctx, kind_ctx)? {
            Type::Bool => Ok(Type::Bool),
            _ => Err(TypeError::NegateNonBool),
        },
        Term::Var(s) => type_ctx
            .lookup(s)
            .ok_or(TypeError::NameError(s.to_string())),
        Term::Abs(param, type_, box body) => {
            let ty = type_.simplify(type_ctx, kind_ctx)?;
            type_ctx.push(param.clone(), ty.clone());
            let result = Ok(Type::Arr(
                Box::new(ty),
                Box::new(typecheck(body, type_ctx, kind_ctx)?),
            ));
            type_ctx.pop();
            result
        }
        Term::KindedTyAbs(param, box body, kind) => {
            type_ctx.push(param.clone(), Type::Var(param.clone()));
            kind_ctx.push(param.clone(), Kind::Star);
            let result = Ok(Type::KindedAll(
                param.clone(),
                Box::new(typecheck(body, type_ctx, kind_ctx)?),
                kind.clone(),
            ));
            type_ctx.pop();
            result
        }
        Term::App(box func, box val) => {
            match typecheck(func, type_ctx, kind_ctx)? {
                Type::Arr(box in_type, box out_type) => {
                    match typecheck(val, type_ctx, kind_ctx)? {
                        ref t if *t == in_type => Ok(out_type.clone()),
                        t => Err(TypeError::ArgMismatch(in_type, t)),
                    }
                }
                _ => Err(TypeError::FuncApp),
            }
        }
        Term::TyApp(box func, ty) => {
            let expected = kindcheck(ty, kind_ctx)
                .map_err(|e| TypeError::KindError(e.to_string()))?;
            match typecheck(func, type_ctx, kind_ctx)? {
                Type::KindedAll(s, box body, kind) => {
                    if kind != expected {
                        Err(TypeError::KindMismatch(expected, kind.clone()))
                    } else {
                        Ok(body.clone().applysubst(&s, ty))
                    }
                }
                _ => Err(TypeError::TyFuncApp),
            }
        }
        Term::Arith(box left, op, box right) => match (
            typecheck(left, type_ctx, kind_ctx)?,
            typecheck(right, type_ctx, kind_ctx)?,
        ) {
            (Type::Int, Type::Int) => Ok(op.return_type()),
            (l, r) => Err(TypeError::Arith(*op, l, r)),
        },
        Term::Logic(box left, op, box right) => match (
            typecheck(left, type_ctx, kind_ctx)?,
            typecheck(right, type_ctx, kind_ctx)?,
        ) {
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (l, r) => Err(TypeError::Logic(*op, l, r)),
        },
        Term::If(box cond, box if_, box else_) => {
            let left = typecheck(if_, type_ctx, kind_ctx)?;
            let right = typecheck(else_, type_ctx, kind_ctx)?;
            if typecheck(cond, type_ctx, kind_ctx)? != Type::Bool {
                Err(TypeError::IfElseCond)
            } else if left != right {
                Err(TypeError::IfElseArms(left, right))
            } else {
                Ok(left)
            }
        }
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, type_ctx, kind_ctx)?;
            type_ctx.push(varname.clone(), val_type);
            let result = Ok(typecheck(term, type_ctx, kind_ctx)?);
            type_ctx.pop();
            result
        }
        Term::Record(fields) => {
            Ok(Type::Record(fields.blabla(typecheck, type_ctx, kind_ctx)?))
        }
        Term::Proj(box term, key) => match typecheck(term, type_ctx, kind_ctx)?
        {
            Type::Record(fields) => fields
                .lookup(&key)
                .ok_or(TypeError::InvalidKey(key.clone())),
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::TyAbs(_, _)
        | Term::InfAbs(_, _)
        | Term::Pack(_, _, _)
        | Term::Unpack(_, _, _, _)
        | Term::BoundedTyAbs(_, _, _)
        | Term::QBool(_)
        | Term::QInt(_)
        | Term::QAbs(_, _, _)
        | Term::QRec(_) => Err(TypeError::Unsupported),
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
            &mut KindContext::empty(),
        ).map(|ty| ty.to_string())
        .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn e2e_type() {
        assert_eq!(
            typecheck_code("(fun[X: *] (x: X) -> x)[Int]"),
            "(Int -> Int)"
        );
        assert_eq!(
            typecheck_code("(fun[X: * -> *] (x: X) -> x)[Int]"),
            "Expected kind of * but got (* -> *)"
        );
    }
}