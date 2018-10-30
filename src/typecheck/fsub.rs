//! Universal Types
// TODO: bounded existentials

use assoclist::TypeContext as Context;
use errors::TypeError;
use syntax::{Substitutable, Term, Type};
use typecheck::simple::Resolve;
use typecheck::sub::is_subtype;

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
        Term::TyAbs(param, box body, bound) => {
            let bound = bound.resolve(ctx)?;
            ctx.push(
                param.clone(),
                Type::BoundedVar(param.clone(), Box::new(bound.clone())),
            );
            let result = Ok(Type::All(
                param.clone(),
                Box::new(typecheck(body, ctx)?),
                Box::new(bound),
            ));
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
        Term::TyApp(box func, argty) => {
            let functy = typecheck(func, ctx).and_then(|ty| {
                ty.expose(ctx).map_err(|s| TypeError::NameError(s))
            })?;
            match functy {
                Type::All(s, box body, box bound) => {
                    if !is_subtype(argty, &bound, ctx) {
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
            let result = typecheck(term, ctx)?;
            ctx.pop();
            Ok(result)
        }
        Term::Record(fields) => {
            Ok(Type::Record(fields.map_typecheck(typecheck, ctx)?))
        }
        Term::Proj(box term, key) => match typecheck(term, ctx)? {
            // TODO: maybe for modules we should have a different err message
            Type::Some(_, _, fields)
            | Type::Record(fields)
            | Type::BoundedVar(_, box Type::Record(fields)) => fields
                .lookup(&key)
                .ok_or(TypeError::InvalidKey(key.clone())),
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::Pack(witness, impls, ty) => {
            let ty = ty.expose(ctx).map_err(|s| TypeError::NameError(s))?;
            if let Type::Some(name, box bound, sigs) = ty {
                if !is_subtype(witness, &bound, ctx) {
                    return Err(TypeError::BoundArgMismatch(
                        bound.clone(),
                        witness.clone(),
                    ));
                }
                let mut expected =
                    sigs.clone().applysubst(&name, witness).resolve(ctx)?;
                let mut actual = impls.map_typecheck(typecheck, ctx)?;
                expected.inner.sort_by_key(|(s, _)| s.clone());
                actual.inner.sort_by_key(|(s, _)| s.clone());
                if actual == expected {
                    Ok(Type::Some(name.clone(), Box::new(bound.clone()), sigs))
                } else {
                    Err(TypeError::ModuleMismatch(expected, actual))
                }
            } else {
                Err(TypeError::ExpectedSome)
            }
        }
        Term::Unpack(tyvar, var, box mod_, box term) => {
            if let Type::Some(hidden, bound, sigs) = typecheck(mod_, ctx)? {
                ctx.push(tyvar.clone(), Type::BoundedVar(tyvar.clone(), bound));
                ctx.push(
                    var.clone(),
                    Type::Record(
                        sigs.clone()
                            .applysubst(&hidden, &Type::Var(tyvar.clone())),
                    ),
                );
                let result = typecheck(term, ctx)?;
                ctx.pop();
                ctx.pop();
                Ok(result)
            } else {
                Err(TypeError::ExpectedSome)
            }
        }
        Term::InfAbs(_, _)
        | Term::QBool(_)
        | Term::QInt(_)
        | Term::QAbs(_, _, _)
        | Term::QRec(_) => Err(TypeError::Unsupported),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::TypeContext as Context;
    use grammar;

    pub fn typecheck_code(code: &str) -> String {
        typecheck(
            &grammar::TermParser::new().parse(code).unwrap(),
            &mut Context::empty(),
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

    #[test]
    fn e2e_bound_exis() {
        assert_eq!(
            typecheck_code(
                "module ops
                    type Bool
                    val toint = fun (x: Bool) -> 0
                end as
                (module sig
                    type X <: Int
                    val toint : X -> Int
                end)"
            ),
            "Expected subtype of Int but got Bool"
        );
        assert_eq!(
            typecheck_code(
                "let mod = module ops
                        type Int
                        val get = 0
                    end as
                    (module sig
                        type X <: Int
                        val get : X
                    end) in
                open mod as c: Inner in
                c.get + 1"
            ),
            "Int"
        );
    }
}
