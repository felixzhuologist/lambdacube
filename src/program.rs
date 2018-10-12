use assoclist::{TermContext, TypeContext};
use errors::TypeError;
use syntax::{Binder, Command, Term, Type};

type TypeChecker = fn(&Term, &mut TypeContext) -> Result<Type, TypeError>;

pub struct Program {
    term_ctx: TermContext,
    ty_ctx: TypeContext,
    typecheck: TypeChecker,
}

impl Program {
    pub const fn new() -> Program {
        Program {
            term_ctx: TermContext::empty(),
            ty_ctx: TypeContext::empty(),
            typecheck: ::typecheck::simple::typecheck,
        }
    }

    pub fn eval(&mut self, code: &str) -> String {
        self.term_ctx = TermContext::empty();
        self.ty_ctx = TypeContext::empty();

        ::grammar::ToplevelParser::new()
            .parse(code)
            .map_err(|err| err.to_string())
            .map(|lines| {
                let mut result = String::new();
                for cmd in lines {
                    match self.eval_cmd(cmd) {
                        Ok(Some((val, ty))) => {
                            result.push_str(&format!("{}: {}\n", val, ty))
                        }
                        Ok(None) => (),
                        Err(err_msg) => return err_msg,
                    }
                }
                result
            }).unwrap_or_else(|err_msg| err_msg)
    }

    pub fn eval_line(&mut self, line: &str) -> String {
        ::grammar::CommandParser::new()
            .parse(line)
            .map_err(|err| err.to_string())
            .and_then(|cmd| self.eval_cmd(cmd))
            .map(|res| {
                res.map_or("".into(), |(t, ty)| format!("{}: {}", t, ty))
            }).unwrap_or_else(|err_msg| err_msg)
    }

    fn eval_cmd(
        &mut self,
        cmd: Command,
    ) -> Result<Option<(Term, Type)>, String> {
        match cmd {
            Command::Binder(binder) => self.eval_binder(binder).map(|_| None),
            Command::Term(ref term) => {
                self.eval_term(term).map(|val| Some(val))
            }
        }
    }

    fn eval_binder(&mut self, binder: Binder) -> Result<(), String> {
        match binder {
            Binder::VarBind(ref s, ref t) => {
                self.eval_term(t).map(|(val, ty)| {
                    self.term_ctx.push(s.clone(), val.clone());
                    self.ty_ctx.push(s.clone(), ty);
                    ()
                })
            }
            Binder::TyBind(_s, _ty) => {
                // TODO
                Ok(())
            }
        }
    }

    fn eval_term(&mut self, ast: &Term) -> Result<(Term, Type), String> {
        (self.typecheck)(ast, &mut self.ty_ctx)
            .map_err(|e| e.to_string())
            .and_then(|type_| {
                ::eval::eval_ast(ast, &mut self.term_ctx)
                    .map(|val| (val, type_))
                    .map_err(|e| e.to_string())
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn update_context() {
        let mut prog = Program::new();
        assert_eq!(prog.eval("let x = 5; x;"), "5: Int\n");
    }
}
