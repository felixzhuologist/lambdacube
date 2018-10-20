//! Universal Types
// TODO: bounded existentials

use assoclist::{Context, TermContext, TypeContext};
use errors::TypeError;
use syntax::{Substitutable, Term, Type};
use typecheck::simple::Resolve;
use typecheck::sub::is_subtype;

pub fn typecheck(
    term: &Term,
    term_ctx: &mut TermContext,
    type_ctx: &mut TypeContext
) -> Result<Type, TypeError> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::Int(_) => Ok(Type::Int),
        Term::Not(box t) => match typecheck(t, term_ctx)? {
            Type::Bool => Ok(Type::Bool),
            _ => Err(TypeError::NegateNonBool),
        },
        Term::Var(s) => {
            term_ctx.get_sort(s).ok_or(TypeError::NameError(s.to_string()))
        }
        Term::Abs(param, type_, box body) => {
            let ty = type_.resolve(term_ctx)?;
            term_ctx.add_sort(param, ty.clone());
            let result =
                Ok(Type::Arr(Box::new(ty), Box::new(typecheck(body, term_ctx)?)));
            term_ctx.pop();
            result
        }
        Term::TyAbs(param, box body) => {
            term_ctx.add_sort(param, Type::Var(param.clone()));
            let result =
                Ok(Type::All(param.clone(), Box::new(typecheck(body, term_ctx)?)));
            term_ctx.pop();
            result
        }
        Term::BoundedTyAbs(param, box body, bound) => {
            let bound = bound.resolve(term_ctx)?;
            term_ctx.add_sort(param.clone(),
                Type::BoundedVar(param.clone(), Box::new(bound.clone())),
            );
            let result = Ok(Type::BoundedAll(
                param.clone(),
                Box::new(typecheck(body, term_ctx)?),
                Box::new(bound),
            ));
            term_ctx.pop();
            result
        }
        Term::App(box func, box val) => match typecheck(func, term_ctx)? {
            Type::Arr(box in_type, box out_type) => {
                let t = typecheck(val, term_ctx)?;
                if is_subtype(&t, &in_type, term_ctx) {
                    Ok(out_type.clone())
                } else {
                    Err(TypeError::ArgMismatch(in_type, t))
                }
            }
            _ => Err(TypeError::FuncApp),
        },
        Term::TyApp(box func, argty) => {
            let functy = typecheck(func, term_ctx).and_then(|ty| {
                ty.expose(term_ctx).map_err(|s| TypeError::NameError(s))
            })?;
            match functy {
                Type::All(s, box body) => {
                    Ok(body.clone().applysubst(&s, argty))
                }
                Type::BoundedAll(s, box body, box bound) => {
                    if !is_subtype(argty, &bound, term_ctx) {
                        Err(TypeError::BoundArgMismatch(
                            bound.clone(),
                            argty.clone(),
                        ))
                    } else {
                        Ok(body.clone().applysubst(&s, argty))
                    }
                }
                _ => Err(TypeError::TyFuncApp),
            }
        }
        Term::Arith(box left, op, box right) => {
            let l = typecheck(left, term_ctx)?;
            let r = typecheck(right, term_ctx)?;
            if is_subtype(&l, &Type::Int, term_ctx)
                && is_subtype(&r, &Type::Int, term_ctx)
            {
                Ok(op.return_type())
            } else {
                Err(TypeError::Arith(*op, l, r))
            }
        }
        Term::Logic(box left, op, box right) => {
            let l = typecheck(left, term_ctx)?;
            let r = typecheck(right, term_ctx)?;
            if is_subtype(&l, &Type::Bool, term_ctx)
                && is_subtype(&r, &Type::Bool, term_ctx)
            {
                Ok(Type::Bool)
            } else {
                Err(TypeError::Logic(*op, l, r))
            }
        }
        Term::If(box cond, box if_, box else_) => {
            let left = typecheck(if_, term_ctx)?;
            let right = typecheck(else_, term_ctx)?;
            if !is_subtype(&typecheck(cond, term_ctx)?, &Type::Bool, term_ctx) {
                Err(TypeError::IfElseCond)
            } else if left != right {
                Err(TypeError::IfElseArms(left, right))
            } else {
                Ok(left)
            }
        }
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, term_ctx)?;
            term_ctx.add_sort(varname, val_type);
            let result = typecheck(term, term_ctx)?;
            term_ctx.pop();
            Ok(result)
        }
        Term::Record(fields) => {
            Ok(Type::Record(fields.map_typecheck(typecheck, term_ctx)?))
        }
        Term::Proj(box term, key) => match typecheck(term, term_ctx)? {
            // TODO: maybe for modules we should have a different err message
            Type::Some(_, fields)
            | Type::Record(fields)
            | Type::BoundedVar(_, box Type::Record(fields)) => fields
                .lookup(&key)
                .ok_or(TypeError::InvalidKey(key.clone())),
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::Pack(witness, impls, ty) => {
            let ty = ty.expose(term_ctx).map_err(|s| TypeError::NameError(s))?;
            if let Type::Some(name, sigs) = ty {
                let mut expected =
                    sigs.clone().applysubst(&name, witness).resolve(term_ctx)?;
                let mut actual = impls.map_typecheck(typecheck, term_ctx)?;
                expected.inner.sort_by_key(|(s, _)| s.clone());
                actual.inner.sort_by_key(|(s, _)| s.clone());
                if actual == expected {
                    Ok(Type::Some(name.clone(), sigs))
                } else {
                    Err(TypeError::ModuleMismatch(expected, actual))
                }
            } else {
                Err(TypeError::ExpectedSome)
            }
        }
        Term::Unpack(tyvar, var, box mod_, box term) => {
            if let Type::Some(_, sigs) = typecheck(mod_, term_ctx)? {
                term_ctx.add_sort(tyvar, Type::Var(tyvar.clone()));
                term_ctx.add_sort(var, Type::Record(sigs.clone()));
                let result = typecheck(term, term_ctx)?;
                term_ctx.pop();
                term_ctx.pop();
                Ok(result)
            } else {
                Err(TypeError::ExpectedSome)
            }
        }
        Term::InfAbs(_, _) => Err(TypeError::Unsupported),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use grammar;

    pub fn typecheck_code(code: &str) -> String {
        typecheck(
            &grammar::TermParser::new().parse(code).unwrap(),
            &mut TermContext::empty(),
            &mut TypeContext::empty(),
        ).map(|ty| ty.to_string())
        .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn e2e_univ() {
        assert_eq!(typecheck_code("fun[X] (x: X) -> x"), "∀X. (X -> X)");
        assert_eq!(
            typecheck_code("fun[X, Y] (f: X -> Y) (x: X) -> f x"),
            "∀X. ∀Y. ((X -> Y) -> (X -> Y))"
        );
        assert_eq!(
            typecheck_code("let f = fun[X] (x: X) -> x in f[Int]"),
            "(Int -> Int)"
        );
        assert_eq!(
            typecheck_code("let f = fun[X] (x: X) -> x in f[Int] 0"),
            "Int"
        );
        assert_eq!(
            typecheck_code("fun[X] (x: X) -> x + 1"),
            "Cannot apply + to X and Int"
        );
    }

    #[test]
    fn e2e_bound_univ() {
        assert_eq!(
            typecheck_code("fun[X <: Int] (x: X) -> x"),
            "∀X <: Int. (X -> X)"
        );

        assert_eq!(
            typecheck_code(
                "fun[X <: Int, Y <: Bool] (f: X -> Y) (x: X) -> f x"
            ),
            "∀X <: Int. ∀Y <: Bool. ((X -> Y) -> (X -> Y))"
        );

        assert_eq!(
            typecheck_code("fun[X <: Int] (x: X) -> x + 1"),
            "∀X <: Int. (X -> Int)"
        );
        assert_eq!(
            typecheck_code("fun[X <: {a: Int}] (x: X) -> {a=x.a, b=x}"),
            "∀X <: {a: Int}. (X -> {a: Int, b: X})"
        );

        assert_eq!(
            typecheck_code("let f = fun[X <: Int] (x: X) -> x in f[Int] 0"),
            "Int"
        );
        assert_eq!(
            typecheck_code(
                "let f = fun[X <: {a: Int}] (x: X) -> x in f[Int] 0"
            ),
            "Expected subtype of {a: Int} but got Int"
        );
        assert_eq!(
            typecheck_code(
                "let f = fun[X <: {a: Int}] (x: X) -> {a=x.a, b=x} in \
                 f[{a: Int, b: Bool}] {a=1, b=false}"
            ),
            "{a: Int, b: {a: Int, b: Bool}}"
        );
    }
}
