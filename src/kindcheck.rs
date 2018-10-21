use assoclist::KindContext;
use errors::KindError;
use syntax::{Kind, Type};

pub fn kindcheck(ty: &Type, ctx: &mut KindContext) -> Result<Kind, KindError> {
    match ty {
        Type::Bool
        | Type::Int
        | Type::Arr(_, _)
        | Type::Record(_)
        | Type::All(_, _)
        | Type::KindedAll(_, _, _)
        | Type::BoundedAll(_, _, _)
        | Type::Some(_, _) => Ok(Kind::Star),
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
    use assoclist::KindContext;
    use grammar;

    pub fn kindcheck_code(code: &str) -> String {
        kindcheck(
            &grammar::TypeParser::new().parse(code).unwrap(),
            &mut KindContext::empty(),
        ).map(|kind| kind.to_string())
        .unwrap_or_else(|err| err.to_string())
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
