use ast::{Expr, Opcode};

pub fn eval_(exp: &Expr) -> i32 {
    match exp {
        Expr::Number(n) => *n,
        Expr::Op(left, op, right) => match op {
            Opcode::Mul => eval_(left) * eval_(right),
            Opcode::Div => eval_(left) / eval_(right),
            Opcode::Add => eval_(left) + eval_(right),
            Opcode::Sub => eval_(left) - eval_(right)
        }
    }
}