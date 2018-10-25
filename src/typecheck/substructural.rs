// TODO: record spread operator (let {a, b, c} = {a=1, b=2, c=3})
// TODO: other qualifiers? (affine, relevant, ordered)

use assoclist::TypeContext as Context;
use errors::TypeError;
use syntax::{Term, Type};
use typecheck::simple::Resolve;

export_kindless_typechecker!(typecheck);

pub fn typecheck(
    term: &Term,
    context: &mut Context,
) -> Result<Type, TypeError> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::QBool(_) => Ok(Type::QBool),
        Term::Int(_) => Ok(Type::Int),
        Term::QInt(_) => Ok(Type::QInt),
        Term::Not(box t) => match typecheck(t, context)? {
            Type::Bool => Ok(Type::Bool),
            _ => Err(TypeError::NegateNonBool),
        },
        Term::Var(s) => context
            .lookup(s)
            .and_then(|ty| {
                if ty.is_qualified() {
                    context.remove(s)
                } else {
                    Some(ty)
                }
            }).ok_or(TypeError::NameError(s.to_string())),
        Term::QAbs(param, type_, box body) => {
            let in_ty = type_.resolve(context)?;
            let out_ty = typecheck_scope(context, param, &in_ty, body)?;
            Ok(Type::QArr(Box::new(in_ty), Box::new(out_ty)))
        }
        Term::Abs(param, type_, box body) => {
            if context.inner.iter().any(|(_, ty)| ty.is_qualified()) {
                // TODO: more helpful error - pass closure maybe?
                return Err(TypeError::AbsContainment);
            }
            let in_ty = type_.resolve(context)?;
            let out_ty = typecheck_scope(context, param, &in_ty, body)?;
            Ok(Type::Arr(Box::new(in_ty), Box::new(out_ty)))
        }
        Term::App(box func, box val) => match typecheck(func, context)? {
            Type::Arr(box in_type, box out_type)
            | Type::QArr(box in_type, box out_type) => {
                match typecheck(val, context)? {
                    ref t if *t == in_type => Ok(out_type.clone()),
                    t => Err(TypeError::ArgMismatch(in_type, t)),
                }
            }
            _ => Err(TypeError::FuncApp),
        },
        Term::Arith(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Int, Type::Int)
                | (Type::QInt, Type::Int)
                | (Type::Int, Type::QInt)
                | (Type::QInt, Type::QInt) => Ok(op.return_type()),
                (l, r) => Err(TypeError::Arith(*op, l, r)),
            }
        }
        Term::Logic(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Bool, Type::Bool)
                | (Type::QBool, Type::Bool)
                | (Type::Bool, Type::QBool)
                | (Type::QBool, Type::QBool) => Ok(Type::Bool),
                (l, r) => Err(TypeError::Logic(*op, l, r)),
            }
        }
        Term::If(box cond, box if_, box else_) => {
            let condtype = typecheck(cond, context)?;
            let mut copied = context.clone();

            let left = typecheck(if_, context)?;
            let right = typecheck(else_, &mut copied)?;
            if condtype != Type::Bool {
                Err(TypeError::IfElseCond)
            } else if left != right {
                Err(TypeError::IfElseArms(left, right))
            } else {
                Ok(left)
            }
        }
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, context)?;
            typecheck_scope(context, varname, &val_type, term)
        }
        Term::Record(fields) => {
            Ok(Type::Record(fields.map_typecheck(typecheck, context)?))
        }
        Term::Proj(box term, key) => match typecheck(term, context)? {
            Type::Record(fields) => fields
                .lookup(&key)
                .ok_or(TypeError::InvalidKey(key.clone())),
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::TyAbs(_, _)
        | Term::TyApp(_, _)
        | Term::InfAbs(_, _)
        | Term::Pack(_, _, _)
        | Term::Unpack(_, _, _, _)
        | Term::KindedTyAbs(_, _, _)
        | Term::BoundedTyAbs(_, _, _)
        | Term::QRec(_) => Err(TypeError::Unsupported),
    }
}

/// Add a variable to the context and evaluate the given term, ensuring that
/// the variable is used the correct number of times. The context will have the
/// same contents after the function returns as it did before calling it
fn typecheck_scope(
    context: &mut Context,
    varname: &String,
    vartype: &Type,
    body: &Term,
) -> Result<Type, TypeError> {
    context.push(varname.clone(), vartype.clone());
    // we don't assume unique names or use de bruijn indices so compare pointers
    let ty_ptr = &context.inner.last().unwrap().1 as *const Type;
    let body_type = match typecheck(body, context) {
        Err(TypeError::NameError(ref s)) if s == varname => {
            Err(TypeError::Linear(varname.clone()))
        }
        result => result,
    }?;

    let is_linear = vartype.is_linear_val();
    let is_unused = context
        .inner
        .last()
        .map_or(false, |(_, ty)| (ty as *const Type) == ty_ptr);

    if is_linear && is_unused {
        return Err(TypeError::Linear(varname.clone()));
    } else if !is_linear {
        context.pop();
    }
    Ok(body_type)
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
        ).map(|ty| ty.to_string())
        .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn e2e_type() {
        assert_eq!(typecheck_code("let x = lin 1 in x + 1"), "Int");
        assert_eq!(
            typecheck_code("let x = lin 1 in x + x"),
            "Linear variable x must be used exactly once"
        );
        // discard function
        assert_eq!(
            typecheck_code(
                "lin fun (x: lin Bool) ->
                    (lin fun (f: Bool -> lin Bool) -> lin true)
                    (fun (y: Bool) -> x)"
            ),
            "Unrestricted functions cannot have linear variables in scope"
        );
        assert_eq!(
            typecheck_code("let x = lin 1 in if true then x + 1 else x - 1"),
            "Int"
        );
        assert_eq!(typecheck_code("let x = 1 in x + x"), "Int");
        assert_eq!(typecheck_code("fun (x: Int) -> x + 1"), "(Int -> Int)");
        assert_eq!(
            typecheck_code("fun (x: lin Int) -> x + 1"),
            "(lin Int -> Int)"
        );
        assert_eq!(
            typecheck_code("fun (x: lin Int) -> x + x"),
            "Linear variable x must be used exactly once"
        );
        assert_eq!(
            typecheck_code("lin fun (x: lin Int) -> x"),
            "lin (lin Int -> lin Int)"
        );
        assert_eq!(
            typecheck_code("fun (x: lin Int) -> 0"),
            "Linear variable x must be used exactly once"
        );
        assert_eq!(
            typecheck_code(
                "let x = lin 0 in let f = lin fun (x: lin Int) -> x in x"
            ),
            "lin Int"
        );
    }
}
