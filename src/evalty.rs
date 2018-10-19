//! Code for evaluating/kindchecking types
use assoclist::{AssocList, KindContext, TypeContext};
use errors::{KindError, TypeError};
use syntax::{Kind, Substitutable, Type};

// use big step semantics since evaluation of types is much simpler
pub fn eval_ty(ty: &Type, ctx: &mut TypeContext) -> Result<Type, TypeError> {
    match ty {
        t @ Type::Bool | t @ Type::Int | t @ Type::TyAbs(_, _, _) => Ok(t.clone()),
        Type::Var(s) | Type::BoundedVar(s, _) => {
            ctx.lookup(s).ok_or(TypeError::NameError(s.to_string()))
        }
        Type::Arr(ref from, ref to) => Ok(Type::Arr(
            Box::new(eval_ty(from, ctx)?),
            Box::new(eval_ty(to, ctx)?),
        )),
        Type::Record(fields) => {
            // TODO: use map + collect
            let mut new_fields = Vec::new();
            for (key, ref ty) in fields.inner.iter() {
                new_fields.push((key.clone(), Box::new(eval_ty(ty, ctx)?)))
            }
            Ok(Type::Record(AssocList::from_vec(new_fields)))
        }
        Type::All(s, ref ty) => {
            ctx.push(s.clone(), Type::Var(s.clone()));
            let result = Ok(Type::All(s.clone(), Box::new(eval_ty(ty, ctx)?)));
            ctx.pop();
            result
        }
        Type::BoundedAll(s, ref ty, ref bound) => {
            ctx.push(s.clone(), Type::Var(s.clone()));
            let result = Ok(Type::BoundedAll(
                s.clone(),
                Box::new(eval_ty(ty, ctx)?),
                Box::new(eval_ty(bound, ctx)?)));
            ctx.pop();
            result
        }
        Type::Some(s, sigs) => {
            ctx.push(s.clone(), Type::Var(s.clone()));
            // TODO: code reuse
            let mut new_sigs = Vec::new();
            for (key, ref ty) in sigs.inner.iter() {
                new_sigs.push((key.clone(), Box::new(eval_ty(ty, ctx)?)))
            }
            ctx.pop();
            Ok(Type::Some(s.clone(), AssocList::from_vec(new_sigs)))
        }
        Type::TyApp(ref func, ref arg) => match eval_ty(func, ctx)? {
            Type::TyAbs(argname, _, box body) => {
                Ok(body.applysubst(&argname, &eval_ty(arg, ctx)?))
            }
            _ => panic!("kindchecking should catch this"),
        },
    }
}

pub fn kindcheck(ty: &Type, ctx: &mut KindContext) -> Result<Kind, KindError> {
    match ty {
        Type::Bool
        | Type::Int
        | Type::Arr(_, _)
        | Type::Record(_)
        | Type::All(_, _)
        | Type::BoundedAll(_, _, _)
        | Type::Some(_, _) => Ok(Kind::Proper),
        Type::Var(s) => {
            ctx.lookup(s).ok_or(KindError::NameError(s.to_string()))
        }
        Type::BoundedVar(_, box ty) => kindcheck(ty, ctx),
        Type::TyAbs(param, kind, box body) => {
            ctx.push(param.clone(), kind.clone());
            let result = Ok(Kind::Arr(
                Box::new(kind.clone()),
                Box::new(kindcheck(body, ctx)?),
            ));
            ctx.pop();
            result
        }
        Type::TyApp(box func, box val) => match kindcheck(func, ctx)? {
            Kind::Arr(box in_kind, box out_kind) => {
                match kindcheck(val, ctx)? {
                    ref t if *t == in_kind => Ok(out_kind.clone()),
                    t => Err(KindError::ArgMismatch(in_kind, t)),
                }
            }
            _ => Err(KindError::FuncApp),
        },
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use assoclist::{KindContext, TypeContext};
    use grammar;

    fn eval_code(code: &str) -> String {
        eval_ty(
            &::grammar::TypeParser::new().parse(code).unwrap(),
            &mut TypeContext::empty(),
        ).map(|ty| ty.to_string())
        .unwrap_or_else(|err| err.to_string())
    }

    pub fn kindcheck_code(code: &str) -> String {
        kindcheck(
            &grammar::TypeParser::new().parse(code).unwrap(),
            &mut KindContext::empty(),
        ).map(|kind| kind.to_string())
        .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn e2e_eval() {
        assert_eq!(eval_code("forall X . X -> X"), "∀X. (X -> X)");
        assert_eq!(eval_code("tyfun (X: *) => X -> X"), "<tyfun>");
        assert_eq!(eval_code("(tyfun (X: *) => X -> X) Int"), "(Int -> Int)");
    }

    #[test]
    fn e2e_type() {
        assert_eq!(kindcheck_code("Int"), "*");
        assert_eq!(kindcheck_code("tyfun (A: *) => A -> A"), "(* -> *)");
        assert_eq!(kindcheck_code("(tyfun (A: *) => A -> A) Int"), "*");
        assert_eq!(
            kindcheck_code("Int Bool"),
            "Tried to apply non function kind"
        );
        assert_eq!(
            kindcheck_code("(tyfun (A: * -> *) => A Int) Int"),
            "Expected argument of kind (* -> *) but got * instead"
        );
    }
}
