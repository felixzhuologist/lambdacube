//! Implementation of let polymorphism with type inference (Hindley-Milner)

use assoclist::TypeContext as Context;
use errors::TypeError;
use syntax::{Term, Type};

type Constraints = Vec<(Type, Type)>;

pub fn typecheck(
    term: &Term,
    context: &mut Context,
) -> Result<Type, TypeError> {
    let (ty, constraints) = get_constraints(term, context)?;
    let sigma = unify(constraints).map_err(|_| TypeError::UnifyError)?;
    applysubst(&ty, &sigma)
}

/// Return (Type, Constraints) pair - the passed in term is of the output type
/// IFF the set of constraints is satisfied
fn get_constraints(
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
            let intype = type_
                .resolve(context)
                .map_err(|s| TypeError::NameError(s))?;

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
            let return_type = Type::Var(pickfreshname(context));
            constr.extend(constr2);
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
        Term::Return(_) => panic!("should not get here"),
    }
}

// TODO: occurs check
fn unify(constr: Constraints) -> Result<Context, ()> {
    let mut result = Context::empty();
    for (left, right) in constr {
        match (left, right) {
            (Type::Var(s), ty) | (ty, Type::Var(s)) => {
                result.push(s, ty);
            }
            (Type::Arr(box ty11, box ty12), Type::Arr(box ty21, box ty22)) => {
                let vals = unify(vec![(ty11, ty21), (ty12, ty22)])?;
                result.inner.extend(vals.inner);
            }
            (ref ty1, ref ty2) if ty1 != ty2 => {
                return Err(())
            }
            (_, _) => ()
        }
    }
    Ok(result)
}

/// Resolve all type variables using the input context
fn applysubst(ty: &Type, ctx: &Context) -> Result<Type, TypeError> {
    match ty {
        Type::Int => Ok(Type::Int),
        Type::Bool => Ok(Type::Bool),
        Type::Arr(ref l, ref r) =>
            Ok(Type::Arr(
                Box::new(applysubst(l, ctx)?),
                Box::new(applysubst(r, ctx)?))),
        Type::Record(_fields) => unimplemented!(),
        Type::Var(s) => ctx.lookup(s).ok_or(TypeError::NameError(s.clone()))
    }
}

fn pickfreshname(ctx: &Context) -> String {
    let mut i = 0;
    loop {
        let var = format!("X?{}", i);
        if !ctx.inner.iter().any(|(s, _)| *s == var) {
            return var;
        }
        i += 1
    }
}
