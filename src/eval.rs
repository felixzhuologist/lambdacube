use std::marker;

use assoclist::{AssocList, KindContext, TermContext, TypeContext};
use errors::{EvalError, TypeError};
use kindcheck::kindcheck;
use syntax::Term::*;
use syntax::{ArithOp, BoolOp, Kind, Substitutable, Term, Type};

pub trait Eval<T, Err> {
    fn eval(&self, ctx: &mut AssocList<String, T>) -> Result<Self, Err>
    where
        T: Clone,
        Self: marker::Sized;
}

pub trait EvalStep<T, Err> {
    fn eval_step(&self, ctx: &mut AssocList<String, T>) -> Result<Self, Err>
    where
        T: Clone,
        Self: marker::Sized;
}

/// Default implementation for Eval which just calls eval_step until it gets stuck
impl<T, Err> Eval<T, Err> for T
where
    T: Clone + EvalStep<T, Err> + PartialEq + Eq,
{
    fn eval(&self, ctx: &mut AssocList<String, T>) -> Result<Self, Err> {
        let mut current = self.clone();
        loop {
            let next = current.eval_step(ctx)?;
            if current == next {
                return Ok(current);
            }
            current = next;
        }
    }
}

impl EvalStep<Term, EvalError> for Term {
    fn eval_step(&self, ctx: &mut TermContext) -> Result<Term, EvalError> {
        match self {
            Not(box Bool(b)) | Not(box QBool(b)) => Ok(Bool(!b)),
            Not(box t) => Ok(Not(Box::new(t.eval_step(ctx)?))),
            App(box Abs(argname, _, box body), box arg)
            | App(box QAbs(argname, _, box body), box arg)
                if arg.is_reduced() =>
            {
                Ok(body.clone().applysubst(&argname, arg))
            }
            App(box InfAbs(argname, box body), box arg) if arg.is_reduced() => {
                Ok(body.clone().applysubst(&argname, arg))
            }
            TyApp(box BoundedTyAbs(_, box body, _), _)
            | TyApp(box KindedTyAbs(_, box body, _), _)
            | TyApp(box TyAbs(_, box body), _) => Ok(body.clone()),
            App(func, box arg) if func.is_val() => {
                Ok(App(func.clone(), Box::new(arg.eval_step(ctx)?)))
            }
            App(box func, arg) => {
                Ok(App(Box::new(func.eval_step(ctx)?), arg.clone()))
            }
            TyApp(box func, arg) => {
                Ok(TyApp(Box::new(func.eval_step(ctx)?), arg.clone()))
            }
            Var(s) => ctx.lookup(s).ok_or(EvalError::NameError(s.to_string())),
            Arith(box Int(a), op, box Int(b))
            | Arith(box QInt(a), op, box Int(b))
            | Arith(box Int(a), op, box QInt(b))
            | Arith(box QInt(a), op, box QInt(b)) => match op {
                ArithOp::Mul => Ok(Int(a * b)),
                ArithOp::Div => Ok(Int(a / b)),
                ArithOp::Add => Ok(Int(a + b)),
                ArithOp::Sub => Ok(Int(a - b)),
                ArithOp::Mod => Ok(Int(a % b)),
                ArithOp::Eq_ => Ok(Bool(a == b)),
                ArithOp::Neq => Ok(Bool(a != b)),
                ArithOp::Gt => Ok(Bool(a > b)),
                ArithOp::Lt => Ok(Bool(a < b)),
                ArithOp::Gte => Ok(Bool(a >= b)),
                ArithOp::Lte => Ok(Bool(a <= b)),
            },
            Arith(left @ box Int(_), op, box right) => Ok(Arith(
                left.clone(),
                op.clone(),
                Box::new(right.eval_step(ctx)?),
            )),
            Arith(box left, op, right) => Ok(Arith(
                Box::new(left.eval_step(ctx)?),
                op.clone(),
                right.clone(),
            )),
            Logic(box Bool(a), op, box Bool(b))
            | Logic(box QBool(a), op, box Bool(b))
            | Logic(box Bool(a), op, box QBool(b))
            | Logic(box QBool(a), op, box QBool(b)) => match op {
                BoolOp::And => Ok(Bool(*a && *b)),
                BoolOp::Or => Ok(Bool(*a || *b)),
            },
            Logic(left @ box Bool(_), op, box right) => Ok(Logic(
                left.clone(),
                op.clone(),
                Box::new(right.eval_step(ctx)?),
            )),
            Logic(box left, op, right) => Ok(Logic(
                Box::new(left.eval_step(ctx)?),
                op.clone(),
                right.clone(),
            )),
            If(box Bool(b), box t1, box t2) => {
                Ok(if *b { t1.clone() } else { t2.clone() })
            }
            If(box cond, t1, t2) => {
                Ok(If(Box::new(cond.eval_step(ctx)?), t1.clone(), t2.clone()))
            }
            Let(varname, box val, box term) if val.is_val() => {
                ctx.push(varname.clone(), val.clone());
                Ok(term.clone())
            }
            Unpack(_, varname, box val, box term) if val.is_val() => {
                match val {
                    Pack(_, impls, _) => {
                        let val = Term::Record(impls.clone());
                        ctx.push(varname.clone(), val.clone());
                        Ok(term.clone())
                    }
                    _ => panic!("type checking should catch this"),
                }
            }
            Let(varname, box val, term) => Ok(Let(
                varname.clone(),
                Box::new(val.eval_step(ctx)?),
                term.clone(),
            )),
            Unpack(ty, var, box val, term) => Ok(Unpack(
                ty.clone(),
                var.clone(),
                Box::new(val.eval_step(ctx)?),
                term.clone(),
            )),
            Record(fields) | QRec(fields) => Ok(Record(fields.eval_step(ctx)?)),
            Proj(box t, key) if t.is_val() => match t {
                Pack(_, fields, _) | Record(fields) => {
                    let key = key.to_string();
                    fields.lookup(&key).ok_or(EvalError::KeyError(key))
                }
                _ => panic!("type checking should catch this"),
            },
            Proj(box term, key) => {
                Ok(Proj(Box::new(term.eval_step(ctx)?), key.clone()))
            }
            Pack(witness, impls, sigs) => {
                Ok(Pack(witness.clone(), impls.eval_step(ctx)?, sigs.clone()))
            }
            _ => Ok(self.clone()),
        }
    }
}

// implement Eval directly for type since it is relatively simple
impl Eval<Type, TypeError> for Type {
    fn eval(&self, ctx: &mut TypeContext) -> Result<Type, TypeError> {
        match self {
            t @ Type::Bool
            | t @ Type::Int
            | t @ Type::Top
            | t @ Type::TyAbs(_, _, _) => Ok(t.clone()),
            Type::Var(s) | Type::BoundedVar(s, _) => {
                ctx.lookup(s).ok_or(TypeError::NameError(s.to_string()))
            }
            Type::Arr(ref from, ref to) => Ok(Type::Arr(
                Box::new(from.eval(ctx)?),
                Box::new(to.eval(ctx)?),
            )),
            Type::Record(fields) => Ok(Type::Record(fields.eval(ctx)?)),
            Type::All(s, ref ty) => {
                ctx.push(s.clone(), Type::Var(s.clone()));
                let result = Ok(Type::All(s.clone(), Box::new(ty.eval(ctx)?)));
                ctx.pop();
                result
            }
            Type::BoundedAll(s, ref ty, ref bound) => {
                ctx.push(s.clone(), Type::Var(s.clone()));
                let result = Ok(Type::BoundedAll(
                    s.clone(),
                    Box::new(ty.eval(ctx)?),
                    Box::new(bound.eval(ctx)?),
                ));
                ctx.pop();
                result
            }
            Type::KindedAll(s, ref ty, kind) => {
                ctx.push(s.clone(), Type::Var(s.clone()));
                let result = Ok(Type::KindedAll(
                    s.clone(),
                    Box::new(ty.eval(ctx)?),
                    kind.clone(),
                ));
                ctx.pop();
                result
            }
            Type::Some(s, sigs) => {
                ctx.push(s.clone(), Type::Var(s.clone()));
                let result = Ok(Type::Some(s.clone(), sigs.eval(ctx)?));
                ctx.pop();
                result
            }
            Type::TyApp(ref func, ref arg) => match func.eval(ctx)? {
                Type::TyAbs(argname, _, box body) => {
                    Ok(body.applysubst(&argname, &arg.eval(ctx)?))
                }
                _ => panic!("kindchecking should catch this"),
            },
        }
    }
}

// TODO: it would be nice if symmetry between Term/Type and Type/Kind was as
// explicit as possible in the code. Right now there are slight differences in
// implementation, e.g. eval_term is a method of Program whereas eval_type is
// its own function here.
pub fn eval_type(
    ty: &Type,
    tyctx: &mut TypeContext,
    kindctx: &mut KindContext,
) -> Result<(Type, Kind), TypeError> {
    kindcheck(ty, kindctx)
        .map_err(|e| TypeError::KindError(e.to_string()))
        .and_then(|kind| ty.eval(tyctx).map(|ty| (ty, kind)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::{TermContext, TypeContext};

    fn eval_term(code: &str) -> String {
        ::grammar::TermParser::new()
            .parse(code)
            .unwrap()
            .eval(&mut TermContext::empty())
            .map(|ty| ty.to_string())
            .unwrap_or_else(|err| err.to_string())
    }

    fn eval_ty_e2e(code: &str) -> String {
        ::grammar::TypeParser::new()
            .parse(code)
            .unwrap()
            .eval(&mut TypeContext::empty())
            .map(|ty| ty.to_string())
            .unwrap_or_else(|err| err.to_string())
    }

    #[test]
    fn check_eval_base() {
        let mut context = TermContext::empty();
        assert_eq!(Int(3).eval_step(&mut context).unwrap(), Int(3));

        context.push("x".to_string(), Int(3));
        assert_eq!(
            Var("x".to_string()).eval_step(&mut context).unwrap(),
            Int(3)
        );
    }

    #[test]
    fn ifelse() {
        assert_eq!(
            If(Box::new(Bool(true)), Box::new(Int(3)), Box::new(Int(5)))
                .eval(&mut TermContext::empty())
                .unwrap(),
            Int(3)
        )
    }

    #[test]
    fn e2e_eval_term() {
        assert_eq!(eval_term("if (3 % 2) == 1 then 10 else 2"), "10");
        assert_eq!(eval_term("1 + 2 + 3 + 4"), "10");
        assert_eq!(eval_term("(fun (x: Int) -> x*4 + 3) 3"), "15");
        assert_eq!(eval_term("let x = 5 in x"), "5");
        assert_eq!(eval_term("{a=1, b=2}"), "{a=1, b=2}");
        assert_eq!(eval_term("{a=2}.a"), "2");
        assert_eq!(
            eval_term("{a=2}.b"),
            "eval error: key b does not exist in record"
        );
        assert_eq!(
            eval_term(
                "let twice = fun (f: Int -> Int) -> f (f 0) in
                 let addone = fun (x: Int) -> x + 1 in
                 twice addone"
            ),
            "2"
        );
        assert_eq!(
            eval_term(
                "let double = fun f a -> f (f a) in
                 let addone = fun x -> x + 1 in
                 let negate = fun b -> not b in
                 let intresult = double addone 0 in
                 let boolresult = double negate true in
                 {b=boolresult, i=intresult}"
            ),
            "{b=true, i=2}"
        );
        assert_eq!(eval_term("(fun b -> not b) true"), "false");
        assert_eq!(eval_term("fun[X] (x: X) -> x"), "<fun>");
        assert_eq!(eval_term("let f = fun[X] (x: X) -> x in f[Int]"), "<fun>");
        assert_eq!(eval_term("let f = fun[X] (x: X) -> x in f[Int] 0"), "0");
        assert_eq!(eval_term("let f = fun[X: *] (x: X) -> x in f[Int] 0"), "0");
        assert_eq!(
            eval_term(
                "let f = fun[X <: {a: Int}] (x: X) -> {a=x, b=(x.a + 1)} \
                 in f[{a: Int, b: Int}]"
            ),
            "<fun>"
        );

        let pack = "module ops
                type Int
                val new = 1
                val get = fun (x: Int) -> x
                val inc = fun (x: Int) -> x + 1
            end as 
            (module sig
                type Counter
                val new : Counter
                val get : Counter -> Int
                val inc : Counter -> Counter
            end)";
        assert_eq!(eval_term(pack), "<mod>");

        let open_use_term = format!(
            "open {} as counter: Counter in counter.get (counter.inc counter.new)",
            pack);
        assert_eq!(eval_term(&open_use_term), "2");

        let open_use_ty = format!(
            "open {} as counter: Counter in fun (c: Counter) -> counter.get c",
            pack
        );
        assert_eq!(eval_term(&open_use_ty), "<fun>");
    }

    #[test]
    fn e2e_eval_ty() {
        assert_eq!(eval_ty_e2e("forall X . X -> X"), "∀X. (X -> X)");
        assert_eq!(eval_ty_e2e("tyfun (X: *) => X -> X"), "<tyfun>");
        assert_eq!(eval_ty_e2e("(tyfun (X: *) => X -> X) Int"), "(Int -> Int)");
    }

    #[test]
    fn substructural() {
        assert_eq!(eval_term("lin 1 + lin 2"), "3");
        assert_eq!(eval_term("(lin fun (x: Int) -> x + 1) 3"), "4");
    }
}
