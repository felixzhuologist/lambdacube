pub enum Expr {
    Number(i32),
    Op(Box<Expr>, Opcode, Box<Expr>),
}

pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub
}
