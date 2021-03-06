//! Universal Types
// TODO: hide opened module types (right now they get resolved to the concrete
// type)

use assoclist::TypeContext as Context;
use errors::TypeError;
use syntax::{Substitutable, Term, Type};
use typecheck::simple::Resolve;

export_kindless_typechecker!(typecheck);

pub fn typecheck(
    term: &Term,
    context: &mut Context,
) -> Result<Type, TypeError> {
    match term {
        Term::Bool(_) => Ok(Type::Bool),
        Term::Int(_) => Ok(Type::Int),
        Term::Not(box t) => match typecheck(t, context)? {
            Type::Bool => Ok(Type::Bool),
            _ => Err(TypeError::NegateNonBool),
        },
        Term::Var(s) => {
            context.lookup(s).ok_or(TypeError::NameError(s.to_string()))
        }
        Term::Abs(param, type_, box body) => {
            let ty = type_.resolve(context)?;
            context.push(param.clone(), ty.clone());
            let result = Ok(Type::Arr(
                Box::new(ty),
                Box::new(typecheck(body, context)?),
            ));
            context.pop();
            result
        }
        Term::TyAbs(param, box body, bound) => {
            context.push(param.clone(), Type::Var(param.clone()));
            let result = Ok(Type::All(
                param.clone(),
                Box::new(typecheck(body, context)?),
                Box::new(bound.clone()),
            ));
            context.pop();
            result
        }
        Term::App(box func, box val) => match typecheck(func, context)? {
            Type::Arr(box in_type, box out_type) => {
                match typecheck(val, context)? {
                    ref t if *t == in_type => Ok(out_type.clone()),
                    t => Err(TypeError::ArgMismatch(in_type, t)),
                }
            }
            _ => Err(TypeError::FuncApp),
        },
        Term::TyApp(box func, ty) => match typecheck(func, context)? {
            Type::All(s, box body, _) => Ok(body.clone().applysubst(&s, ty)),
            _ => Err(TypeError::TyFuncApp),
        },
        Term::Arith(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Int, Type::Int) => Ok(op.return_type()),
                (l, r) => Err(TypeError::Arith(*op, l, r)),
            }
        }
        Term::Logic(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Bool, Type::Bool) => Ok(Type::Bool),
                (l, r) => Err(TypeError::Logic(*op, l, r)),
            }
        }
        Term::If(box cond, box if_, box else_) => {
            let left = typecheck(if_, context)?;
            let right = typecheck(else_, context)?;
            if typecheck(cond, context)? != Type::Bool {
                Err(TypeError::IfElseCond)
            } else if left != right {
                Err(TypeError::IfElseArms(left, right))
            } else {
                Ok(left)
            }
        }
        Term::Let(varname, box val, box term) => {
            let val_type = typecheck(val, context)?;
            context.push(varname.clone(), val_type);
            let result = typecheck(term, context)?;
            context.pop();
            Ok(result)
        }
        Term::Record(fields) => {
            Ok(Type::Record(fields.map_typecheck(typecheck, context)?))
        }
        Term::Proj(box term, key) => match typecheck(term, context)? {
            // TODO: maybe for modules we should have a different err message
            Type::Some(_, _, fields) | Type::Record(fields) => fields
                .lookup(&key)
                .ok_or(TypeError::InvalidKey(key.clone())),
            _ => Err(TypeError::ProjectNonRecord),
        },
        Term::Pack(witness, impls, ty) => {
            // expose first since we can't apply substituion or try resolving
            // the type until we know it's a Type::Some
            let ty = ty.expose(context).map_err(|s| TypeError::NameError(s))?;
            if let Type::Some(name, bound, sigs) = ty {
                let mut expected =
                    sigs.clone().applysubst(&name, witness).resolve(context)?;
                let mut actual = impls
                    .map_typecheck(typecheck, context)?
                    .resolve(context)?;
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
            if let Type::Some(hidden, _, sigs) = typecheck(mod_, context)? {
                context.push(tyvar.clone(), Type::Var(tyvar.clone()));
                context.push(
                    var.clone(),
                    Type::Record(
                        sigs.clone()
                            .applysubst(&hidden, &Type::Var(tyvar.clone())),
                    ),
                );
                let result = typecheck(term, context)?;
                context.pop();
                context.pop();
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
        assert_eq!(
            typecheck_code("fun (f: forall X . X -> X) -> f[Int] 3"),
            "(∀X. (X -> X) -> Int)"
        );

        let pair =
            "fun (x: Int) (y: Int) -> (fun[X] (z: Int -> Int -> X) -> z x y)";
        let pairtype = "∀X. ((Int -> (Int -> X)) -> X)";
        assert_eq!(
            typecheck_code(pair),
            format!("(Int -> (Int -> {}))", pairtype)
        );

        let fst = format!(
            "fun (p: {}) -> p[Int] (fun (x: Int) (y: Int) -> x)",
            pairtype
        );
        assert_eq!(typecheck_code(&fst), format!("({} -> Int)", pairtype));

        let snd = format!(
            "fun (p: {}) -> p[Int] (fun (x: Int) (y: Int) -> y)",
            pairtype
        );
        assert_eq!(typecheck_code(&snd), format!("({} -> Int)", pairtype));

        assert_eq!(typecheck_code(&format!("({}) 1 2", pair)), pairtype);
        assert_eq!(
            typecheck_code(&format!("({}) (({}) 1 2)", fst, pair)),
            "Int"
        );
        assert_eq!(
            typecheck_code(&format!("({}) (({}) 1 2)", snd, pair)),
            "Int"
        );
    }

    #[test]
    fn e2e_exis() {
        let module = "
            module sig
                type Counter
                val new : Counter
                val get : Counter -> Int
                val inc : Counter -> Counter
            end";

        let pack = format!(
            "module ops
                type Int
                val new = 1
                val get = fun (x: Int) -> x
                val inc = fun (x: Int) -> x + 1
            end as ({})",
            module
        );
        assert_eq!(
            typecheck_code(&pack),
            "∃Counter. new: Counter, get: (Counter -> Int), \
             inc: (Counter -> Counter)"
        );

        let open_use_term = format!(
            "open {} as counter: Counter in counter.get (counter.inc counter.new)",
            pack);
        assert_eq!(typecheck_code(&open_use_term), "Int");

        let open_use_ty = format!(
            "open {} as counter: Counter in fun (c: Counter) -> counter.get c",
            pack
        );
        assert_eq!(typecheck_code(&open_use_ty), "(Counter -> Int)");

        // check that assigning a different name to the hidden type works
        let open_use_ty_alias = format!(
            "open {} as counter: C in fun (c: C) -> counter.get c",
            pack
        );
        assert_eq!(typecheck_code(&open_use_ty_alias), "(C -> Int)");

        // check that we can't use the hidden type concretely
        let use_ty_concrete =
            format!("open {} as counter: C in counter.new + 1", pack);
        assert_eq!(
            typecheck_code(&use_ty_concrete),
            "Cannot apply + to C and Int"
        );

        let modty = grammar::TypeParser::new().parse(module).unwrap();
        let mut ctx = Context::empty();
        ctx.push(String::from("CounterADT"), *modty);
        let ast = grammar::TermParser::new()
            .parse("fun (c: CounterADT) -> c.get (c.inc c.new)")
            .unwrap();
        let result = typecheck(&ast, &mut ctx)
            .map(|ty| ty.to_string())
            .unwrap_or_else(|err| err.to_string());
        assert_eq!(
            result,
            "(∃Counter. new: Counter, get: (Counter -> Int), \
             inc: (Counter -> Counter) -> Int)"
        );
    }
}
