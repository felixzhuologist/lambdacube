use ast::{Expr, Opcode};

pub fn eval_ast(exp: &Expr) -> i32 {
    match exp {
        Expr::Number(n) => *n,
        Expr::Op(left, op, right) => match op {
            Opcode::Mul => eval_ast(left) * eval_ast(right),
            Opcode::Div => eval_ast(left) / eval_ast(right),
            Opcode::Add => eval_ast(left) + eval_ast(right),
            Opcode::Sub => eval_ast(left) - eval_ast(right)
        }
    }
}