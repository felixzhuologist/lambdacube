use assoclist::TypeContext as Context;
use errors::TypeError;
use syntax::{Kind, Substitutable, Term, Type};
use typecheck::simple::Resolve;

export_kindless_typechecker!(typecheck);

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
        Term::Abs(param, type_, box body) => {
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
            Ok(Type::Record(fields.map_typecheck(typecheck, ctx)?))
        }
        Term::Proj(box term, key) => match typecheck(term, ctx)? {
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

// TODO: implement Ord for Types?
// TODO: return error for lookup failures instead of false?
pub fn is_subtype(left: &Type, right: &Type, ctx: &mut Context) -> bool {
    left == right || match (left, right) {
        (_, Type::Top) => true,
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
            Type::All(ref s1, box ty1, ref bound1),
            Type::All(ref s2, box ty2, ref bound2),
        )
            if bound1 == bound2 =>
        {
            let tyfun1 = ty1.clone().applysubst(s1, bound1);
            let tyfun2 = ty2.clone().applysubst(s2, bound2);
            is_subtype(&tyfun1, &tyfun2, ctx)
        }
        (Type::TyAbs(s1, k1, box ty1), Type::TyAbs(s2, k2, box ty2)) => {
            if k1 != k2 {
                return false;
            }
            let placeholder_type = get_top(k1);
            ctx.push(s1.clone(), placeholder_type.clone());
            ctx.push(s2.clone(), placeholder_type);
            let result = is_subtype(ty1, ty2, ctx);
            ctx.pop();
            ctx.pop();
            result
        }
        _ => false,
    }
}

/// Returns the supertype with the given kind
pub fn get_top(kind: &Kind) -> Type {
    match kind {
        Kind::Star => Type::Top,
        Kind::Arr(box from, box to) => {
            Type::TyAbs(String::from("_"), from.clone(), Box::new(get_top(to)))
        }
    }
}

/// Converts output of get_top() back to a kind
pub fn get_kind(ty: &Type) -> Kind {
    match ty {
        Type::Top => Kind::Star,
        Type::TyAbs(_, from, box to) => {
            Kind::Arr(Box::new(from.clone()), Box::new(get_kind(to)))
        }
        _ => panic!("should not get here"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::{AssocList, TypeContext};

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
            Type::Int,
        )]));
        let big_rec = Type::Record(AssocList::from_vec(vec![
            ("a".to_string(), Type::Int),
            ("b".to_string(), Type::Bool),
        ]));
        assert!(is_subtype(&big_rec, &small_rec, &mut ctx));

        // depth subtyping
        let shallow = Type::Record(AssocList::from_vec(vec![(
            "a".to_string(),
            small_rec.clone(),
        )]));
        let deep = Type::Record(AssocList::from_vec(vec![(
            "a".to_string(),
            big_rec.clone(),
        )]));
        assert!(is_subtype(&shallow, &deep, &mut ctx));

        // function subtyping is contravariant for input, covariant for output
        let f1 =
            Type::Arr(Box::new(small_rec.clone()), Box::new(shallow.clone()));
        let f2 = Type::Arr(Box::new(big_rec.clone()), Box::new(deep.clone()));
        assert!(is_subtype(&f1, &f2, &mut ctx));

        let poly1 = Type::All(
            String::from("X"),
            Box::new(big_rec.clone()),
            Box::new(Type::Int),
        );
        let poly2 = Type::All(
            String::from("X"),
            Box::new(small_rec.clone()),
            Box::new(Type::Int),
        );
        assert!(is_subtype(&poly1, &poly2, &mut ctx));
    }
}
