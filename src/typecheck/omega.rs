use assoclist::{KindContext, TypeContext};
use errors::TypeError;
use eval;
use std::marker;
use syntax::{Kind, Term, Type};

// TODO: make star kind by default, or mention in docs that kinds must be provided
// explicitly
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
            let ty = type_.simplify(param, type_ctx, kind_ctx)?;
            type_ctx.push(param.clone(), ty.clone());
            let result = Ok(Type::Arr(
                Box::new(ty),
                Box::new(typecheck(body, type_ctx, kind_ctx)?),
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
        Term::TyAbs(_, _, _)
        | Term::TyApp(_, _)
        | Term::InfAbs(_, _)
        | Term::Pack(_, _, _)
        | Term::Unpack(_, _, _, _)
        | Term::QBool(_)
        | Term::QInt(_)
        | Term::QAbs(_, _, _)
        | Term::QRec(_) => Err(TypeError::Unsupported),
    }
}

pub trait Simplify {
    fn simplify(
        &self,
        varname: &String,
        type_ctx: &mut TypeContext,
        kind_ctx: &mut KindContext,
    ) -> Result<Self, TypeError>
    where
        Self: marker::Sized;
}

impl Simplify for Type {
    fn simplify(
        &self,
        varname: &String,
        type_ctx: &mut TypeContext,
        kind_ctx: &mut KindContext,
    ) -> Result<Self, TypeError> {
        eval::eval_type(self, type_ctx, kind_ctx).and_then(|(ty, kind)| {
            match kind {
                Kind::Star => Ok(ty),
                k => Err(TypeError::NonProper(varname.clone(), k)),
            }
        })
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
        let tyfun = "tyfun (X: *) => X -> X";
        assert_eq!(
            typecheck_code(&format!("fun (x: {}) -> x", tyfun)),
            "Term x must have kind * but has kind (* -> *)"
        );
        assert_eq!(
            typecheck_code(&format!("fun (x: ({}) Int) -> x", tyfun)),
            "((Int -> Int) -> (Int -> Int))"
        );
    }
}
