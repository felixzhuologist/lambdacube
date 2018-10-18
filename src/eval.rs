use assoclist::{AssocList, TermContext as Context};
use errors::EvalError;
use syntax::Term::*;
use syntax::{ArithOp, BoolOp, Substitutable, Term};

/// call eval step on a term until it is stuck
pub fn eval_ast(term: &Term, ctx: &mut Context) -> Result<Term, EvalError> {
    let mut current = term.clone();
    loop {
        let next = eval_step(&current, ctx)?;
        if current == next {
            return Ok(current);
        }
        current = next;
    }
}

pub fn eval_step(
    term: &Term,
    context: &mut Context,
) -> Result<Term, EvalError> {
    match term {
        Not(box Bool(b)) => Ok(Bool(!b)),
        Not(box t) => Ok(Not(Box::new(eval_step(t, context)?))),
        App(box Abs(argname, _, box body), box arg) if arg.is_reduced() => {
            Ok(body.clone().applysubst(&argname, arg))
        }
        App(box InfAbs(argname, box body), box arg) if arg.is_reduced() => {
            Ok(body.clone().applysubst(&argname, arg))
        }
        // TODO: does eval care about the type substitution?
        TyApp(box BoundedTyAbs(_, box body, _), _)
        | TyApp(box TyAbs(_, box body), _) => Ok(body.clone()),
        App(func, box arg) if func.is_val() => {
            Ok(App(func.clone(), Box::new(eval_step(arg, context)?)))
        }
        App(box func, arg) => {
            Ok(App(Box::new(eval_step(func, context)?), arg.clone()))
        }
        TyApp(box func, arg) => {
            Ok(TyApp(Box::new(eval_step(func, context)?), arg.clone()))
        }
        Var(s) => context.lookup(s).ok_or(EvalError::NameError(s.to_string())),
        Arith(box Int(a), op, box Int(b)) => match op {
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
            Box::new(eval_step(right, context)?),
        )),
        Arith(box left, op, right) => Ok(Arith(
            Box::new(eval_step(left, context)?),
            op.clone(),
            right.clone(),
        )),
        Logic(box Bool(a), op, box Bool(b)) => match op {
            BoolOp::And => Ok(Bool(*a && *b)),
            BoolOp::Or => Ok(Bool(*a || *b)),
        },
        Logic(left @ box Bool(_), op, box right) => Ok(Logic(
            left.clone(),
            op.clone(),
            Box::new(eval_step(right, context)?),
        )),
        Logic(box left, op, right) => Ok(Logic(
            Box::new(eval_step(left, context)?),
            op.clone(),
            right.clone(),
        )),
        If(box Bool(b), box t1, box t2) => {
            Ok(if *b { t1.clone() } else { t2.clone() })
        }
        If(box cond, t1, t2) => Ok(If(
            Box::new(eval_step(cond, context)?),
            t1.clone(),
            t2.clone(),
        )),
        Let(varname, box val, box term) if val.is_val() => {
            context.push(varname.clone(), val.clone());
            Ok(term.clone())
        }
        Unpack(_, varname, box val, box term) if val.is_val() => match val {
            Pack(_, impls, _) => {
                let val = Term::Record(impls.clone());
                context.push(varname.clone(), val.clone());
                Ok(term.clone())
            }
            _ => panic!("type checking should catch this"),
        },
        Let(varname, box val, term) => Ok(Let(
            varname.clone(),
            Box::new(eval_step(val, context)?),
            term.clone(),
        )),
        Unpack(ty, var, box val, term) => Ok(Unpack(
            ty.clone(),
            var.clone(),
            Box::new(eval_step(val, context)?),
            term.clone(),
        )),
        Record(fields) => {
            // TODO: should probably step one field at a time instead of stepping
            // them all forward at once
            let mut new_fields = Vec::new();
            for (key, box val) in fields.inner.iter() {
                new_fields
                    .push((key.clone(), Box::new(eval_step(val, context)?)))
            }
            Ok(Record(AssocList::from_vec(new_fields)))
        }
        Proj(box t, key) if t.is_val() => match t {
            Pack(_, fields, _) | Record(fields) => {
                let key = key.to_string();
                fields
                    .lookup(&key)
                    .map(|v| *v)
                    .ok_or(EvalError::KeyError(key))
            }
            _ => panic!("type checking should catch this"),
        },
        Proj(box term, key) => {
            Ok(Proj(Box::new(eval_step(term, context)?), key.clone()))
        }
        Pack(witness, impls, sigs) => {
            // TODO: code reuse
            let mut new_fields = Vec::new();
            for (key, box val) in impls.inner.iter() {
                new_fields
                    .push((key.clone(), Box::new(eval_step(val, context)?)))
            }
            Ok(Pack(
                witness.clone(),
                AssocList::from_vec(new_fields),
                sigs.clone(),
            ))
        }
        _ => Ok(term.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval_code(code: &str) -> String {
        match eval_ast(
            &::grammar::TermParser::new().parse(code).unwrap(),
            &mut Context::empty(),
        ) {
            Ok(ast) => ast.to_string(),
            Err(err) => err.to_string(),
        }
    }

    #[test]
    fn check_eval_base() {
        let mut context = Context::empty();
        assert_eq!(eval_step(&Int(3), &mut context).unwrap(), Int(3));

        context.push("x".to_string(), Int(3));
        assert_eq!(
            eval_step(&Var("x".to_string()), &mut context).unwrap(),
            Int(3)
        );
    }

    #[test]
    fn ifelse() {
        assert_eq!(
            eval_ast(
                &If(Box::new(Bool(true)), Box::new(Int(3)), Box::new(Int(5))),
                &mut Context::empty()
            ).unwrap(),
            Int(3)
        )
    }

    #[test]
    fn e2e_eval() {
        assert_eq!(eval_code("if (3 % 2) == 1 then 10 else 2"), "10");
        assert_eq!(eval_code("1 + 2 + 3 + 4"), "10");
        assert_eq!(eval_code("(fun (x: Int) -> x*4 + 3) 3"), "15");
        assert_eq!(eval_code("let x = 5 in x"), "5");
        assert_eq!(eval_code("{a=1, b=2}"), "{a=1, b=2}");
        assert_eq!(eval_code("{a=2}.a"), "2");
        assert_eq!(
            eval_code("{a=2}.b"),
            "eval error: key b does not exist in record"
        );
        assert_eq!(
            eval_code(
                "let twice = fun (f: Int -> Int) -> f (f 0) in
                 let addone = fun (x: Int) -> x + 1 in
                 twice addone"
            ),
            "2"
        );
        assert_eq!(
            eval_code(
                "let double = fun f a -> f (f a) in
                 let addone = fun x -> x + 1 in
                 let negate = fun b -> not b in
                 let intresult = double addone 0 in
                 let boolresult = double negate true in
                 {b=boolresult, i=intresult}"
            ),
            "{b=true, i=2}"
        );
        assert_eq!(eval_code("(fun b -> not b) true"), "false");
        assert_eq!(eval_code("fun[X] (x: X) -> x"), "<fun>");
        assert_eq!(eval_code("let f = fun[X] (x: X) -> x in f[Int]"), "<fun>");
        assert_eq!(eval_code("let f = fun[X] (x: X) -> x in f[Int] 0"), "0");
        assert_eq!(
            eval_code(
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
        assert_eq!(eval_code(pack), "<mod>");

        let open_use_term = format!(
            "open {} as counter: Counter in counter.get (counter.inc counter.new)",
            pack);
        assert_eq!(eval_code(&open_use_term), "2");

        let open_use_ty = format!(
            "open {} as counter: Counter in fun (c: Counter) -> counter.get c",
            pack
        );
        assert_eq!(eval_code(&open_use_ty), "<fun>");
    }
}
