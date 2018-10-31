// TODO: kinded existentials
// TODO: make * kind implicit for type parameters? also error messages can get
// confusing since fun[A, B] is valid syntax but will error at the type checking
// stage as "unsupported type feature". it's unclear that the cause of the error
// is that kinds must be provided explicitly here

use assoclist::{KindContext, TypeContext};
use errors::TypeError;
use eval;
use eval::Eval;
use kindcheck::kindcheck;
use syntax::{Kind, Substitutable, Term, Type};
use typecheck::omega::Simplify;
use typecheck::sub::get_kind;

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
            type_ctx.push(param.clone(), Type::Var(param.clone()));
            kind_ctx.push(param.clone(), get_kind(bound));
            let result = Ok(Type::All(
                param.clone(),
                Box::new(typecheck(body, type_ctx, kind_ctx)?),
                Box::new(bound.clone()),
            ));
            type_ctx.pop();
            kind_ctx.pop();
            result
        }
        Term::App(box func, box val) => {
            match typecheck(func, type_ctx, kind_ctx)? {
                Type::Arr(box in_type, box out_type) => {
                    match typecheck(val, type_ctx, kind_ctx)? {
                        ref t if *t == in_type => Ok(out_type.clone()),
                        t => Err(TypeError::ArgMismatch(in_type, t)),
                    }
                }
                _ => Err(TypeError::FuncApp),
            }
        }
        Term::TyApp(box func, ty) => {
            let actual = kindcheck(ty, kind_ctx)
                .map_err(|e| TypeError::KindError(e.to_string()))?;
            match typecheck(func, type_ctx, kind_ctx)? {
                Type::All(s, box body, box bound) => {
                    let expected = get_kind(&bound);
                    if expected != actual {
                        Err(TypeError::KindMismatch(expected, actual))
                    } else {
                        Ok(body.clone().applysubst(&s, ty).eval(type_ctx)?)
                    }
                }
                _ => Err(TypeError::TyFuncApp),
            }
        }
        Term::Arith(box left, op, box right) => match (
            typecheck(left, type_ctx, kind_ctx)?,
            typecheck(right, type_ctx, kind_ctx)?,
        ) {
            (Type::Int, Type::Int) => Ok(op.return_type()),
            (l, r) => Err(TypeError::Arith(*op, l, r)),
        },
        Term::Logic(box left, op, box right) => match (
            typecheck(left, type_ctx, kind_ctx)?,
            typecheck(right, type_ctx, kind_ctx)?,
        ) {
            (Type::Bool, Type::Bool) => Ok(Type::Bool),
            (l, r) => Err(TypeError::Logic(*op, l, r)),
        },
        Term::If(box cond, box if_, box else_) => {
            let left = typecheck(if_, type_ctx, kind_ctx)?;
            let right = typecheck(else_, type_ctx, kind_ctx)?;
            if typecheck(cond, type_ctx, kind_ctx)? != Type::Bool {
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
            Type::Record(fields) | Type::Some(_, _, fields) => fields
                .lookup(&key)
                .ok_or(TypeError::InvalidKey(key.clone())),
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::Pack(witness, impls, ty) => {
            let ty = ty.eval(type_ctx)?;
            if let Type::Some(name, bound, sigs) = ty {
                let bound_kind = get_kind(&bound);
                let (witness, witness_kind) =
                    eval::eval_type(witness, type_ctx, kind_ctx)?;
                if let Kind::Arr(_, _) = bound_kind {
                    return Err(TypeError::ModuleKind);
                } else if witness_kind != bound_kind {
                    return Err(TypeError::KindMismatch(
                        bound_kind,
                        witness_kind,
                    ));
                }

                let mut expected =
                    sigs.clone().applysubst(&name, &witness).eval(type_ctx)?;
                let mut actual = impls
                    .map_typecheck_kind(typecheck, type_ctx, kind_ctx)?
                    .eval(type_ctx)?;
                expected.inner.sort_by_key(|(s, _)| s.clone());
                actual.inner.sort_by_key(|(s, _)| s.clone());
                if actual == expected {
                    Ok(Type::Some(name.clone(), bound.clone(), sigs))
                } else {
                    Err(TypeError::ModuleMismatch(expected, actual))
                }
            } else {
                Err(TypeError::ExpectedSome)
            }
        }
        Term::Unpack(tyvar, var, box mod_, box term) => {
            if let Type::Some(hidden, _, sigs) =
                typecheck(mod_, type_ctx, kind_ctx)?
            {
                type_ctx.push(tyvar.clone(), Type::Var(tyvar.clone()));
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
    use assoclist::TypeContext;
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
        assert_eq!(
            typecheck_code("(fun[X: * -> *] (x: X Int) -> x)[Int]"),
            "Expected kind of (* -> *) but got *"
        );
        assert_eq!(
            typecheck_code(
                "(fun[X: * -> *] (x: X Int) -> x)[tyfun (X: *) => {x: X}]"
            ),
            "({x: Int} -> {x: Int})"
        );

        assert_eq!(
            typecheck_code(
                "module ops
                    type Int
                    val toint = fun (x: Int) -> x
                end as
                (module sig
                    type X: *
                    val toint : X -> Int
                end)"
            ),
            "∃X. toint: (X -> Int)"
        );

        assert_eq!(
            typecheck_code(
                "module ops
                    type (tyfun (X: *) => {x: X})
                    val toint = fun (rec: {x: X}) -> rec.x
                end as
                (module sig
                    type X: * -> *
                    val toint : X Int -> Int
                end)"
            ),
            "Module hidden type must have kind *"
        );
    }

    #[test]
    fn churchpair() {
        let pairtype =
            "tyfun (Fst: *) (Snd: *) => ∀X. (Fst -> Snd -> X) -> X";
        let pair =
            "fun[Fst, Snd] (x: Fst) (y: Snd) -> (fun[X] (z: Fst -> Snd -> X) -> z x y)";
        assert_eq!(
            typecheck_code(pair),
            "∀Fst. ∀Snd. (Fst -> (Snd -> ∀X. ((Fst -> (Snd -> X)) -> X)))");

        let fst = format!(
            "fun[Fst, Snd] (p: ({}) Fst Snd) -> p[Fst] (fun (x: Fst) (y: Snd) -> x)",
            pairtype);
        assert_eq!(
            typecheck_code(&fst),
            "∀Fst. ∀Snd. (∀X. ((Fst -> (Snd -> X)) -> X) -> Fst)"
        );

        let snd = format!(
            "fun[Fst, Snd] (p: ({}) Fst Snd) -> p[Snd] (fun (x: Fst) (y: Snd) -> y)",
            pairtype);
        assert_eq!(
            typecheck_code(&snd),
            "∀Fst. ∀Snd. (∀X. ((Fst -> (Snd -> X)) -> X) -> Snd)"
        );

        assert_eq!(
            typecheck_code(&format!("({})[Int, Bool] 1 true", pair)),
            "∀X. ((Int -> (Bool -> X)) -> X)"
        );
        assert_eq!(
            typecheck_code(&format!(
                "({})[Int, Bool] (({})[Int, Bool] 1 true)",
                fst, pair
            )),
            "Int"
        );
        assert_eq!(
            typecheck_code(&format!(
                "({})[Int, Bool] (({})[Int, Bool] 1 true)",
                snd, pair
            )),
            "Bool"
        );
    }

    #[test]
    fn counterclass() {
        let counterm = "tyfun (R: *) => {get: R -> Int, inc: R -> R}";
        let obj = "tyfun (M: * -> *) =>
                        module sig
                            type X
                            val state : X
                            val methods : M X
                        end";
        let counterty = format!("({}) ({})", obj, counterm);
        let counter = format!(
            "module ops
                type {{x: Int}}
                val state = {{x=5}}
                val methods = {{
                    get = fun (r: {{x: Int}}) -> r.x,
                    inc = fun (r: {{x: Int}}) -> {{x=r.x + 1}}
                }}
            end as ({})",
            counterty
        );
        assert_eq!(
            typecheck_code(&counter),
            "∃X. state: X, methods: <tyfun> X"
        );
    }
}
