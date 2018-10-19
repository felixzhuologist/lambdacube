//! Implementation of let polymorphism with type inference (Hindley-Milner)
use assoclist::TypeContext as Context;
use errors::TypeError;
use eval::Eval;
use syntax::{Term, Type};

pub type Constraints = Vec<(Type, Type)>;

pub fn typecheck(
    term: &Term,
    context: &mut Context,
) -> Result<Type, TypeError> {
    let (ty, constraints) = get_constraints(term, context)?;
    let sigma = unify(constraints).map_err(|_| TypeError::UnifyError)?;
    Ok(applysubst(ty, &sigma))
}

/// Return (Type, Constraints) pair - the passed in term is of the output type
/// IFF the set of constraints is satisfied
pub fn get_constraints(
    term: &Term,
    context: &mut Context,
) -> Result<(Type, Constraints), TypeError> {
    match term {
        Term::Bool(_) => Ok((Type::Bool, Vec::new())),
        Term::Int(_) => Ok((Type::Int, Vec::new())),
        Term::Not(box t) => {
            Ok((Type::Bool, vec![(typecheck(t, context)?, Type::Bool)]))
        }
        Term::Var(s) => match context.lookup(s) {
            Some(type_) => Ok((type_, Vec::new())),
            None => Err(TypeError::NameError(s.to_string())),
        },
        Term::Abs(param, box type_, box body) => {
            let intype = type_.eval(context).unwrap_or(type_.clone());

            context.push(param.clone(), intype.clone());
            let (outtype, constr) = get_constraints(body, context)?;
            let result =
                Ok((Type::Arr(Box::new(intype), Box::new(outtype)), constr));
            context.pop();
            result
        }
        Term::InfAbs(param, box body) => {
            let intype = Type::Var(pick_fresh_intype(&context));
            context.push(param.clone(), intype.clone());
            let (outtype, constr) = get_constraints(body, context)?;
            let result =
                Ok((Type::Arr(Box::new(intype), Box::new(outtype)), constr));
            context.pop();
            result
        }
        Term::App(box func, box val) => {
            let (ty1, mut constr) = get_constraints(func, context)?;
            let (ty2, constr2) = get_constraints(val, context)?;
            constr.extend(constr2);
            let return_type = Type::Var(pick_fresh_outtype(&constr));
            constr.push((
                ty1,
                Type::Arr(Box::new(ty2), Box::new(return_type.clone())),
            ));
            Ok((return_type, constr))
        }
        // TODO: refactor shared code
        Term::Arith(box left, op, box right) => {
            let (tyleft, mut constr) = get_constraints(left, context)?;
            let (tyright, mut constr2) = get_constraints(right, context)?;
            constr.extend(constr2);
            constr.push((tyleft, Type::Int));
            constr.push((tyright, Type::Int));

            Ok((op.return_type(), constr))
        }
        Term::Logic(box left, _, box right) => {
            let (tyleft, mut constr) = get_constraints(left, context)?;
            let (tyright, mut constr2) = get_constraints(right, context)?;
            constr.extend(constr2);
            constr.push((tyleft, Type::Bool));
            constr.push((tyright, Type::Bool));

            Ok((Type::Bool, constr))
        }
        Term::If(box cond, box if_, box else_) => {
            let (tycond, mut constr) = get_constraints(cond, context)?;
            let (tyif, constr1) = get_constraints(if_, context)?;
            let (tyelse, constr2) = get_constraints(else_, context)?;
            constr.extend(constr1);
            constr.extend(constr2);
            constr.push((tycond, Type::Bool));
            constr.push((tyif.clone(), tyelse));

            Ok((tyif, constr))
        }
        Term::Let(varname, box val, box term) => {
            let (tyval, mut constr) = get_constraints(val, context)?;
            context.push(varname.clone(), tyval);
            let (tyres, constr2) = get_constraints(term, context)?;
            constr.extend(constr2);
            Ok((tyres, constr))
        }
        Term::Record(_) | Term::Proj(_, _) => unimplemented!(),
        Term::TyAbs(_, _)
        | Term::TyApp(_, _)
        | Term::Pack(_, _, _)
        | Term::Unpack(_, _, _, _)
        | Term::BoundedTyAbs(_, _, _) => Err(TypeError::Unsupported),
    }
}

fn unify(mut constr: Constraints) -> Result<Context, ()> {
    let mut result = Context::empty();
    while !constr.is_empty() {
        match constr.pop().unwrap() {
            (ref ty1, ref ty2) if ty1 == ty2 => (),
            (Type::Var(s), ty) | (ty, Type::Var(s)) => {
                if occursin(&s, &ty) {
                    return Err(());
                }
                constr = update_constraints(&s, &ty, constr);
                result.push(s, ty);
            }
            (Type::Arr(box ty11, box ty12), Type::Arr(box ty21, box ty22)) => {
                constr.insert(0, (ty11, ty21));
                constr.insert(0, (ty12, ty22));
            }
            (_, _) => return Err(()),
        }
    }
    Ok(result)
}

/// Resolve all type variables using the input context
pub fn applysubst(ty: Type, ctx: &Context) -> Type {
    ctx.inner
        .iter()
        .fold(ty, |ty, (s, tyval)| tysubst(s, tyval, ty))
}

pub fn update_constraints(
    s: &str,
    ty: &Type,
    constr: Constraints,
) -> Constraints {
    constr
        .into_iter()
        .map(|(l, r)| (tysubst(&s, &ty, l), tysubst(&s, &ty, r)))
        .collect()
}

fn pick_fresh_intype(ctx: &Context) -> String {
    let mut var = String::from("X");
    loop {
        if !ctx
            .inner
            .iter()
            .any(|(s, ty2)| *s == var || occursin(&var, ty2))
        {
            return var;
        }
        var.push('\'');
    }
}

fn pick_fresh_outtype(constr: &Constraints) -> String {
    let mut i = 0;
    loop {
        let var = format!("X?{}", i);
        if !constr
            .iter()
            .any(|(ty1, ty2)| occursin(&var, ty1) || occursin(&var, ty2))
        {
            return var;
        }
        i += 1
    }
}

fn tysubst(s: &str, tyout: &Type, tyin: Type) -> Type {
    match tyin {
        Type::Int => Type::Int,
        Type::Bool => Type::Bool,
        Type::Arr(box l, box r) => Type::Arr(
            Box::new(tysubst(&s, &tyout, l)),
            Box::new(tysubst(&s, &tyout, r)),
        ),
        Type::Record(_fields) => unimplemented!(),
        Type::Var(name) => if s == name {
            tyout.clone()
        } else {
            Type::Var(name)
        },
        // other types don't exist in HM and this function isn't used elsewhere
        _ => unimplemented!(),
    }
}

pub fn occursin(s: &str, ty: &Type) -> bool {
    match ty {
        Type::Int => false,
        Type::Bool => false,
        Type::Arr(ref l, ref r) => occursin(&s, l) || occursin(&s, r),
        Type::Record(_fields) => unimplemented!(),
        Type::Var(name) => name == s,
        // other types don't exist in HM and this function isn't used elsewhere
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::TypeContext;
    use grammar;

    fn typecheck_code(code: &str) -> String {
        match typecheck(
            &grammar::TermParser::new().parse(code).unwrap(),
            &mut TypeContext::empty(),
        ) {
            Ok(type_) => type_.to_string(),
            Err(err) => err.to_string(),
        }
    }

    #[test]
    fn check_infer() {
        assert_eq!(typecheck_code("fun x -> x + 1"), "(Int -> Int)");
        assert_eq!(typecheck_code("fun (x: X) -> x"), "(X -> X)");
        assert_eq!(typecheck_code("fun x -> x"), "(X -> X)");
        assert_eq!(
            typecheck_code("fun (z: Z -> Z) -> fun (y: Y -> Y) -> z (y true)"),
            "((Bool -> Bool) -> ((Bool -> Bool) -> Bool))"
        );
        assert_eq!(
            typecheck_code("fun (z: Z -> Z) (y: Y -> Y) -> z (y true)"),
            "((Bool -> Bool) -> ((Bool -> Bool) -> Bool))"
        );
        assert_eq!(
            typecheck_code("fun z -> fun y -> z (y true)"),
            "((X?0 -> X?1) -> ((Bool -> X?0) -> X?1))"
        );
        assert_eq!(
            typecheck_code("fun z y -> z (y true)"),
            "((X?0 -> X?1) -> ((Bool -> X?0) -> X?1))"
        );
    }
}
