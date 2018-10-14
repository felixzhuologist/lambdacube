use std::fmt;
use syntax::{ArithOp, BoolOp, Type};

#[derive(Debug)]
pub enum EvalError {
    NameError(String),
    KeyError(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EvalError::NameError(ref s) => {
                write!(f, "eval error: variable {} not found", s)
            }
            EvalError::KeyError(ref s) => {
                write!(f, "eval error: key {} does not exist in record", s)
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    NegateNonBool,
    NameError(String),
    ArgMismatch(Type, Type),
    FuncApp,
    TyFuncApp,
    Arith(ArithOp, Type, Type),
    Logic(BoolOp, Type, Type),
    IfElseCond,
    IfElseArms(Type, Type),
    InvalidKey(String),
    ProjectNonRecord,
    UnifyError,
    // Some terms/types are only supported in certain cases (for example,
    // the InfAbs is only valid for HM since that's currently the only system
    // that can do type inference). It should probably be a syntax error to
    // try and use something that is unsupported, but that would require having
    // multiple parsers which is expensive because the generated parsers are
    // large. So instead, the typechecker returns an Unsupported error for now
    Unsupported,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeError::NegateNonBool => {
                write!(f, "Attempted to negate non boolean value")
            }
            TypeError::NameError(ref s) => {
                write!(f, "Variable {} not found", s)
            }
            TypeError::ArgMismatch(ref expected, ref actual) => write!(
                f,
                "Expected argument of type {} got but {} instead",
                expected, actual
            ),
            TypeError::FuncApp => write!(f, "Tried to apply non function type"),
            TypeError::TyFuncApp => write!(f, "Tried to apply non type function type"),
            TypeError::Arith(ref op, ref left, ref right) => {
                write!(f, "Cannot apply {} to {} and {}", op, left, right)
            }
            TypeError::Logic(ref op, ref left, ref right) => {
                write!(f, "Cannot apply {} to {} and {}", op, left, right)
            }
            TypeError::IfElseCond => {
                write!(f, "If/else condition must be a Bool")
            }
            TypeError::IfElseArms(ref left, ref right) => write!(
                f,
                "If/else arms have mismatched types: {} and {}",
                left, right
            ),
            TypeError::InvalidKey(ref s) => {
                write!(f, "Record does not have key {}", s)
            }
            TypeError::ProjectNonRecord => {
                write!(f, "Can only project record types")
            }
            TypeError::UnifyError => write!(f, "Type error"),
            // TODO: this shoudl probably be a syntax error
            TypeError::Unsupported => {
                write!(f, "Attempted to use an unsupported feature for the current type system")
            }
        }
    }
}
