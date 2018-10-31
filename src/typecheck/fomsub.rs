// TODO: kinded existentials

use assoclist::{KindContext, TypeContext};
use errors::TypeError;
use eval::Eval;
use kindcheck::kindcheck;
use syntax::{Kind, Substitutable, Term, Type};
use typecheck::omega::Simplify;
use typecheck::sub::is_subtype;

pub fn typecheck(
    term: &Term,
    type_ctx: &mut TypeContext,
    kind_ctx: &mut KindContext,
) -> Result<Type, TypeError> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::Int(_) => Ok(Type::Int),
        Term::Not(box t) => match typecheck(t, type_ctx, kind_ctx)? {
            Type::Bool => Ok(Type::Bool),
            _ => Err(TypeError::NegateNonBool),
        },
        Term::Var(s) => type_ctx
            .lookup(s)
            .ok_or(TypeError::NameError(s.to_string())),
        Term::Abs(param, type_, box body) => {
            let ty = type_.simplify(param, type_ctx, kind_ctx)?;
            type_ctx.push(param.clone(), ty.clone());
            let result = Ok(Type::Arr(
                Box::new(ty),
                Box::new(typecheck(body, type_ctx, kind_ctx)?),
            ));
            type_ctx.pop();
            result
        }
        Term::TyAbs(param, box body, bound) => {
            let bound = bound.eval(type_ctx)?;
            type_ctx.push(
                param.clone(),
                Type::BoundedVar(param.clone(), Box::new(bound.clone())),
            );
            let bound_kind = kindcheck(&bound, kind_ctx)
                .map_err(|e| TypeError::KindError(e.to_string()))?;
            kind_ctx.push(param.clone(), bound_kind);
            let result = Ok(Type::All(
                param.clone(),
                Box::new(typecheck(body, type_ctx, kind_ctx)?),
                Box::new(bound),
            ));
            type_ctx.pop();
            kind_ctx.pop();
            result
        }
        Term::App(box func, box val) => {
            match typecheck(func, type_ctx, kind_ctx)? {
                Type::Arr(box in_type, box out_type) => {
                    let t = typecheck(val, type_ctx, kind_ctx)?;
                    if is_subtype(&t, &in_type, type_ctx) {
                        Ok(out_type.clone())
                    } else {
                        Err(TypeError::ArgMismatch(in_type, t))
                    }
                }
                _ => Err(TypeError::FuncApp),
            }
        }
        Term::TyApp(box func, ty) => {
            kindcheck(ty, kind_ctx)
                .map_err(|e| TypeError::KindError(e.to_string()))?;

            let functy =
                typecheck(func, type_ctx, kind_ctx).and_then(|ty| {
                    ty.expose(type_ctx).map_err(|s| TypeError::NameError(s))
                })?;
            match functy {
                Type::All(s, box body, box bound) => {
                    if !is_subtype(ty, &bound, type_ctx) {
                        Err(TypeError::BoundArgMismatch(
                            bound.clone(),
                            ty.clone(),
                        ))
                    } else {
                        Ok(body.clone().applysubst(&s, ty).eval(type_ctx)?)
                    }
                }
                _ => Err(TypeError::TyFuncApp),
            }
        }
        Term::Arith(box left, op, box right) => {
            let l = typecheck(left, type_ctx, kind_ctx)?;
            let r = typecheck(right, type_ctx, kind_ctx)?;
            if is_subtype(&l, &Type::Int, type_ctx)
                && is_subtype(&r, &Type::Int, type_ctx)
            {
                Ok(op.return_type())
            } else {
                Err(TypeError::Arith(*op, l, r))
            }
        }
        Term::Logic(box left, op, box right) => {
            let l = typecheck(left, type_ctx, kind_ctx)?;
            let r = typecheck(right, type_ctx, kind_ctx)?;
            if is_subtype(&l, &Type::Bool, type_ctx)
                && is_subtype(&r, &Type::Bool, type_ctx)
            {
                Ok(Type::Bool)
            } else {
                Err(TypeError::Logic(*op, l, r))
            }
        }
        Term::If(box cond, box if_, box else_) => {
            let left = typecheck(if_, type_ctx, kind_ctx)?;
            let right = typecheck(else_, type_ctx, kind_ctx)?;
            if !is_subtype(
                &typecheck(cond, type_ctx, kind_ctx)?,
                &Type::Bool,
                type_ctx,
            ) {
                Err(TypeError::IfElseCond)
            } else if left != right {
                Err(TypeError::IfElseArms(left, right))
            } else {
                Ok(left)
            }
        }
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, type_ctx, kind_ctx)?;
            type_ctx.push(varname.clone(), val_type);
            let result = Ok(typecheck(term, type_ctx, kind_ctx)?);
            type_ctx.pop();
            result
        }
        Term::Record(fields) => Ok(Type::Record(
            fields.map_typecheck_kind(typecheck, type_ctx, kind_ctx)?,
        )),
        Term::Proj(box term, key) => match typecheck(term, type_ctx, kind_ctx)?
        {
            Type::Some(_, _, fields)
            | Type::Record(fields)
            | Type::BoundedVar(_, box Type::Record(fields))
            | Type::BoundedVar(_, box Type::Some(_, _, fields)) => fields
                .lookup(&key)
                .ok_or(TypeError::InvalidKey(key.clone())),
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::Pack(witness, impls, ty) => {
            let ty = ty.eval(type_ctx)?;
            if let Type::Some(name, box bound, sigs) = ty {
                if !is_subtype(witness, &bound, type_ctx) {
                    return Err(TypeError::BoundArgMismatch(
                        bound.clone(),
                        witness.clone(),
                    ));
                }

                let mut expected =
                    sigs.clone().applysubst(&name, witness).eval(type_ctx)?;
                let mut actual = impls
                    .map_typecheck_kind(typecheck, type_ctx, kind_ctx)?
                    .eval(type_ctx)?;
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
            if let Type::Some(hidden, bound, sigs) =
                typecheck(mod_, type_ctx, kind_ctx)?
            {
                type_ctx.push(
                    tyvar.clone(),
                    Type::BoundedVar(tyvar.clone(), bound),
                );
                kind_ctx.push(tyvar.clone(), Kind::Star);
                type_ctx.push(
                    var.clone(),
                    Type::Record(
                        sigs.clone()
                            .applysubst(&hidden, &Type::Var(tyvar.clone())),
                    ),
                );
                kind_ctx.push(var.clone(), Kind::Star);
                let result = typecheck(term, type_ctx, kind_ctx)?;
                type_ctx.pop();
                type_ctx.pop();
                kind_ctx.pop();
                kind_ctx.pop();
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
pub mod tests {
    use super::*;
    use assoclist::{KindContext, TypeContext};
    use grammar;

    pub fn typecheck_code(code: &str) -> String {
        typecheck(
            &grammar::TermParser::new().parse(code).unwrap(),
            &mut TypeContext::empty(),
            &mut KindContext::empty(),
        ).map(|ty| ty.to_string())
        .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn e2e_type() {
        assert_eq!(
            typecheck_code("(fun[X: *] (x: X) -> x)[Int]"),
            "(Int -> Int)"
        );
        assert_eq!(
            typecheck_code("(fun[X: * -> *] (x: X) -> x)[Int]"),
            "Term x must have type with kind * but has one with kind (* -> *)"
        );
        // TODO: this error message isn't great...
        assert_eq!(
            typecheck_code("(fun[X: * -> *] (x: X Int) -> x)[Int]"),
            "Expected subtype of <tyfun> but got Int"
        );
        assert_eq!(
            typecheck_code(
                "(fun[X: * -> *] (x: X Int) -> x)[tyfun (X: *) => {x: X}]"
            ),
            "({x: Int} -> {x: Int})"
        );

        assert_eq!(
            typecheck_code("(fun[X <: Top] (x: X) -> x)[Int]"),
            "(Int -> Int)"
        );
        assert_eq!(
            typecheck_code("(fun[X <: (Top -> Top)] (x: X) -> x)[Int]"),
            "Expected subtype of (Top -> Top) but got Int"
        );

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

    #[test]
    fn exis_ret() {
        let module = "module sig type T val inc: T -> T end";
        let func = "fun (c: Counter) -> open c as body: X in
            module ops
                type X
                val inc = body.inc
            end as (Counter)";

        let modty = grammar::TypeParser::new().parse(module).unwrap();
        let mut type_ctx = TypeContext::empty();
        let mut kind_ctx = KindContext::empty();
        type_ctx.push(String::from("Counter"), *modty);
        kind_ctx.push(String::from("Counter"), Kind::Star);
        let ast = grammar::TermParser::new().parse(func).unwrap();
        let result = typecheck(&ast, &mut type_ctx, &mut kind_ctx)
            .map(|ty| ty.to_string())
            .unwrap_or_else(|err| err.to_string());
        assert_eq!(result, "(∃T. inc: (T -> T) -> ∃T. inc: (T -> T))");
    }
}
