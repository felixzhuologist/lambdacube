use assoclist::{AssocList, TermContext as Context};
use errors::EvalError;
use syntax::ArithOp;
use syntax::BoolOp;
use syntax::Term;
use syntax::Term::*;

/// call eval step on a term until it is stuck
pub fn eval_ast(term: &Term) -> Result<Term, EvalError> {
    let mut current = term.clone();
    let mut context: Context = Context::empty();

    loop {
        let next = eval_step(&current, &mut context)?;
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
        App(box Abs(argname, _, body), box arg) if arg.is_reduced() => {
            context.push(argname.clone(), arg.clone());
            Ok(Return(body.clone()))
        }
        App(func, box arg) if func.is_val() => {
            Ok(App(func.clone(), Box::new(eval_step(arg, context)?)))
        }
        App(box func, arg) => {
            Ok(App(Box::new(eval_step(func, context)?), arg.clone()))
        }
        Return(box term) if (*term).is_val() => {
            context.pop();
            Ok(term.clone())
        }
        Return(box term) => Ok(Return(Box::new(eval_step(term, context)?))),
        Var(s) => match context.lookup(s) {
            Some(val) => Ok(val),
            None => Err(EvalError::NameError(s.to_string())),
        },
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
        Let(varname, box val, term) => Ok(Let(
            varname.clone(),
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
            Record(fields) => {
                let key = key.to_string();
                match fields.lookup(&key) {
                    Some(val) => Ok(*val),
                    None => Err(EvalError::KeyError(key)),
                }
            }
            _ => panic!("type checking should catch this"),
        },
        Proj(box term, key) => {
            Ok(Proj(Box::new(eval_step(term, context)?), key.clone()))
        }
        _ => Ok(term.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::TermContext;
    use syntax::Type;

    fn eval_code(code: &str) -> String {
        match eval_ast(&::grammar::TermParser::new().parse(code).unwrap()) {
            Ok(ast) => ast.to_string(),
            Err(err) => err.to_string(),
        }
    }

    #[test]
    fn check_eval_base() {
        let mut context: TermContext = TermContext::empty();
        assert_eq!(eval_step(&Int(3), &mut context).unwrap(), Int(3));

        context.push("x".to_string(), Int(3));
        assert_eq!(
            eval_step(&Var("x".to_string()), &mut context).unwrap(),
            Int(3)
        );
    }

    #[test]
    fn check_eval_func() {
        let x = "x".to_string();
        let body = Box::new(Var(x.clone()));
        let id = Abs(x.clone(), Box::new(Type::Int), body.clone());

        let app = App(Box::new(id.clone()), Box::new(Int(33)));
        assert_eq!(eval_ast(&id).unwrap(), id);

        let mut context = TermContext::empty();
        let mut curr = eval_step(&app, &mut context).unwrap();
        assert_eq!(curr, Return(body));
        assert_eq!(*context.peek().unwrap(), (x.clone(), Int(33)));

        curr = eval_step(&curr, &mut context).unwrap();
        assert_eq!(curr, Return(Box::new(Int(33))));
        assert_eq!(*context.peek().unwrap(), (x.clone(), Int(33)));

        curr = eval_step(&curr, &mut context).unwrap();
        assert_eq!(curr, Int(33));
        assert!(context.peek().is_none());

        assert_eq!(eval_ast(&app).unwrap(), Int(33));
    }

    #[test]
    fn ifelse() {
        assert_eq!(
            eval_ast(&If(
                Box::new(Bool(true)),
                Box::new(Int(3)),
                Box::new(Int(5))
            )).unwrap(),
            Int(3)
        )
    }

    #[test]
    fn e2e_eval() {
        assert_eq!(eval_code("if (3 % 2) = 1 then 10 else 2"), "10");
        assert_eq!(eval_code("1 + 2 + 3 + 4"), "10");
        assert_eq!(eval_code("(fun x: Int . x*4 + 3) 3"), "15");
        assert_eq!(eval_code("let x = 5 in x"), "5");
        assert_eq!(eval_code("{a=1, b=2}"), "{a=1, b=2}");
        assert_eq!(eval_code("{a=2}.a"), "2");
        assert_eq!(
            eval_code("{a=2}.b"),
            "eval error: key b does not exist in record"
        );
    }
}
