use super::hm::{
    applysubst, get_constraints, occursin, update_constraints, Constraints,
};
use super::sub::is_subtype;
use assoclist::TypeContext as Context;
use errors::TypeError;
use syntax::{Term, Type};

pub fn typecheck(
    term: &Term,
    context: &mut Context,
) -> Result<Type, TypeError> {
    let (ty, constraints) = get_constraints(term, context)?;
    let sigma = unify(constraints).map_err(|_| TypeError::UnifyError)?;
    Ok(applysubst(ty, &sigma))
}

pub fn unify(mut constr: Constraints) -> Result<Context, ()> {
    let mut result = Context::empty();
    while !constr.is_empty() {
        match constr.pop().unwrap() {
            (ref ty1, ref ty2)
                if is_subtype(ty1, ty2) || is_subtype(ty2, ty1) =>
            {
                ()
            }
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
