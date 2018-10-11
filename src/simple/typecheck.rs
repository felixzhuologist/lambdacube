use assoclist::{AssocList, TypeContext as Context};
use errors::TypeError;
use syntax::ArithOp::*;
use syntax::{Term, Type};

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
        Term::Var(s) => match context.lookup(s) {
            Some(type_) => Ok(type_),
            None => Err(TypeError::NameError(s.to_string())),
        },
        Term::Abs(param, box type_, box body) => {
            context.push(param.clone(), type_.clone());
            let result = Ok(Type::Arr(
                Box::new(type_.clone()),
                Box::new(typecheck(body, context)?),
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
        Term::Arith(box left, op, box right) => {
            match (typecheck(left, context)?, typecheck(right, context)?) {
                (Type::Int, Type::Int) => match op {
                    Mul | Div | Add | Sub | Mod => Ok(Type::Int),
                    Eq_ | Neq | Gt | Lt | Gte | Lte => Ok(Type::Bool),
                },
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
            let result = Ok(typecheck(term, context)?);
            context.pop();
            result
        }
        Term::Record(fields) => {
            let mut types = Vec::new();
            for (key, box val) in fields.inner.iter() {
                types.push((key.clone(), Box::new(typecheck(val, context)?)))
            }
            Ok(Type::Record(AssocList::from_vec(types)))
        }
        Term::Proj(box term, key) => match typecheck(term, context)? {
            Type::Record(fields) => match fields.lookup(&key) {
                Some(type_) => Ok(*type_),
                None => Err(TypeError::InvalidKey(key.clone())),
            },
            _ => Err(TypeError::ProjectNonRecord),
        },
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assoclist::TypeContext;
    use grammar;

    fn typecheck_code(code: &str) -> String {
        match typecheck(
            &grammar::TermParser::new().parse(code).unwrap(),
            &mut TypeContext::empty(),
        ) {
            Ok(type_) => type_.to_string(),
            Err(err) => err.to_string(),
        }
    }

    #[test]
    fn e2e_type() {
        assert_eq!(typecheck_code("if true then 0 else 2"), "Int");
        assert_eq!(
            typecheck_code("let x := 0 in if x + 2 then 0 else 2"),
            "If/else condition must be a Bool"
        );
        assert_eq!(typecheck_code("fun x: Int . x >= 0"), "Int -> Bool");
        assert_eq!(typecheck_code("3 4"), "Tried to apply non function type");
        assert_eq!(typecheck_code("(fun x: Int . x*2 + 1) 0"), "Int");
    }
}
