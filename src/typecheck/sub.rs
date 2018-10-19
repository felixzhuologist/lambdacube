use assoclist::{AssocList, TypeContext as Context};
use errors::TypeError;
use syntax::{Substitutable, Term, Type};
use typecheck::simple::Resolve;

pub fn typecheck(term: &Term, ctx: &mut Context) -> Result<Type, TypeError> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::Int(_) => Ok(Type::Int),
        Term::Not(box t) => match typecheck(t, ctx)? {
            Type::Bool => Ok(Type::Bool),
            _ => Err(TypeError::NegateNonBool),
        },
        Term::Var(s) => {
            ctx.lookup(s).ok_or(TypeError::NameError(s.to_string()))
        }
        Term::Abs(param, box type_, box body) => {
            let ty = type_.resolve(ctx)?;
            ctx.push(param.clone(), ty.clone());
            let result =
                Ok(Type::Arr(Box::new(ty), Box::new(typecheck(body, ctx)?)));
            ctx.pop();
            result
        }
        Term::App(box func, box val) => match typecheck(func, ctx)? {
            Type::Arr(box in_type, box out_type) => {
                let t = typecheck(val, ctx)?;
                if is_subtype(&t, &in_type, ctx) {
                    Ok(out_type.clone())
                } else {
                    Err(TypeError::ArgMismatch(in_type, t))
                }
            }
            _ => Err(TypeError::FuncApp),
        },
        Term::Arith(box left, op, box right) => {
            let l = typecheck(left, ctx)?;
            let r = typecheck(right, ctx)?;
            if is_subtype(&l, &Type::Int, ctx)
                && is_subtype(&r, &Type::Int, ctx)
            {
                Ok(op.return_type())
            } else {
                Err(TypeError::Arith(*op, l, r))
            }
        }
        Term::Logic(box left, op, box right) => {
            let l = typecheck(left, ctx)?;
            let r = typecheck(right, ctx)?;
            if is_subtype(&l, &Type::Bool, ctx)
                && is_subtype(&r, &Type::Bool, ctx)
            {
                Ok(Type::Bool)
            } else {
                Err(TypeError::Logic(*op, l, r))
            }
        }
        Term::If(box cond, box if_, box else_) => {
            let left = typecheck(if_, ctx)?;
            let right = typecheck(else_, ctx)?;
            if !is_subtype(&typecheck(cond, ctx)?, &Type::Bool, ctx) {
                Err(TypeError::IfElseCond)
            } else if left != right {
                Err(TypeError::IfElseArms(left, right))
            } else {
                Ok(left)
            }
        }
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, ctx)?;
            ctx.push(varname.clone(), val_type);
            let result = Ok(typecheck(term, ctx)?);
            ctx.pop();
            result
        }
        Term::Record(fields) => {
            let mut types = Vec::new();
            for (key, box val) in fields.inner.iter() {
                types.push((key.clone(), Box::new(typecheck(val, ctx)?)))
            }
            Ok(Type::Record(AssocList::from_vec(types)))
        }
        Term::Proj(box term, key) => match typecheck(term, ctx)? {
            Type::Record(fields) => match fields.lookup(&key) {
                Some(type_) => Ok(*type_),
                None => Err(TypeError::InvalidKey(key.clone())),
            },
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::TyAbs(_, _)
        | Term::TyApp(_, _)
        | Term::InfAbs(_, _)
        | Term::Pack(_, _, _)
        | Term::Unpack(_, _, _, _)
        | Term::BoundedTyAbs(_, _, _) => Err(TypeError::Unsupported),
    }
}

// TODO: implement Ord for Types?
// TODO: return error for lookup failures instead of false?
pub fn is_subtype(left: &Type, right: &Type, ctx: &mut Context) -> bool {
    left == right || match (left, right) {
        (Type::Record(fields1), Type::Record(fields2)) => fields2
            .inner
            .iter()
            .all(|(key, ltype)| match fields1.lookup(key) {
                Some(ref rtype) => is_subtype(ltype, rtype, ctx),
                None => false,
            }),
        (Type::Arr(in1, out1), Type::Arr(in2, out2)) => {
            is_subtype(in2, in1, ctx) && is_subtype(out1, out2, ctx)
        }
        (Type::Var(ref s), ty) => {
            let promoted = ctx.lookup(s);
            if promoted.is_some() {
                match promoted.unwrap() {
                    Type::Var(ref s2) if s == s2 => false,
                    ref result => is_subtype(result, ty, ctx),
                }
            } else {
                false
            }
        }
        (Type::BoundedVar(_, ref bound), ty) => is_subtype(bound, ty, ctx),
        // TODO: this assumes that the two bounds are already exposed
        (
            Type::BoundedAll(ref s1, box ty1, ref bound1),
            Type::BoundedAll(ref s2, box ty2, ref bound2),
        )
            if bound1 == bound2 =>
        {
            let tyfun1 = ty1.clone().applysubst(s1, bound1);
            let tyfun2 = ty2.clone().applysubst(s2, bound2);
            is_subtype(&tyfun1, &tyfun2, ctx)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::TypeContext;

    #[test]
    fn subtyping() {
        let mut ctx = TypeContext::empty();

        // types are equal
        assert!(is_subtype(&Type::Bool, &Type::Bool, &mut ctx));
        assert!(is_subtype(
            &Type::Arr(Box::new(Type::Int), Box::new(Type::Bool)),
            &Type::Arr(Box::new(Type::Int), Box::new(Type::Bool)),
            &mut ctx
        ));

        // width subtyping
        let small_rec = Type::Record(AssocList::from_vec(vec![(
            "a".to_string(),
            Box::new(Type::Int),
        )]));
        let big_rec = Type::Record(AssocList::from_vec(vec![
            ("a".to_string(), Box::new(Type::Int)),
            ("b".to_string(), Box::new(Type::Bool)),
        ]));
        assert!(is_subtype(&big_rec, &small_rec, &mut ctx));

        // depth subtyping
        let shallow = Type::Record(AssocList::from_vec(vec![(
            "a".to_string(),
            Box::new(small_rec.clone()),
        )]));
        let deep = Type::Record(AssocList::from_vec(vec![(
            "a".to_string(),
            Box::new(big_rec.clone()),
        )]));
        assert!(is_subtype(&shallow, &deep, &mut ctx));

        // function subtyping is contravariant for input, covariant for output
        let f1 =
            Type::Arr(Box::new(small_rec.clone()), Box::new(shallow.clone()));
        let f2 = Type::Arr(Box::new(big_rec.clone()), Box::new(deep.clone()));
        assert!(is_subtype(&f1, &f2, &mut ctx));

        let poly1 = Type::BoundedAll(
            String::from("X"),
            Box::new(big_rec.clone()),
            Box::new(Type::Int),
        );
        let poly2 = Type::BoundedAll(
            String::from("X"),
            Box::new(small_rec.clone()),
            Box::new(Type::Int),
        );
        assert!(is_subtype(&poly1, &poly2, &mut ctx));
    }
}
