use assoclist::AssocList;
use std::fmt;
use syntax::{ArithOp, BoolOp, Kind, Type};

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum TypeError {
    NegateNonBool,
    NameError(String),
    ArgMismatch(Type, Type),
    BoundArgMismatch(Type, Type),
    KindMismatch(Kind, Kind),
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
    ExpectedSome,
    ModuleMismatch(AssocList<String, Type>, AssocList<String, Type>),
    ModuleKind,
    /// Pass off a KindError err message as a TypeError
    // TODO: is there a better way to do this?
    KindError(String),
    // Tried to use a value that has a non proper type
    NonProper(String, Kind),
    AbsContainment,
    RecContainment,
    Linear(String),
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
                "Expected argument of type {} but got {} instead",
                expected, actual
            ),
            TypeError::BoundArgMismatch(ref expected, ref actual) => write!(
                f,
                "Expected subtype of {} but got {}",
                expected, actual
            ),
            TypeError::KindMismatch(ref expected, ref actual) => write!(
                f,
                "Expected kind of {} but got {}",
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
            TypeError::ExpectedSome => {
                write!(f, "Expected an existential type")
            }
            TypeError::ModuleMismatch(ref expected, ref actual) => {
                write!(
                    f,
                    "Your module implementation does not match the declared \
                    type signature. Your implementation has type:\n{}\nBut \
                    the declared interface is of type:\n{}",
                    actual,
                    expected)
            }
            TypeError::ModuleKind => {
                write!(f, "Module hidden type must have kind *")
            }
            TypeError::KindError(ref s) => write!(f, "{}", s),
            TypeError::NonProper(ref v, ref k) => {
                write!(f, "Term {} must have type with kind * but has one with kind {}", v, k)
            }
            TypeError::AbsContainment => write!(f, "Unrestricted functions cannot have linear variables in scope"),
            TypeError::RecContainment => write!(f, "Unrestricted records cannot have fields with linear values"),
            TypeError::Linear(ref s) => write!(f, "Linear variable {} must be used exactly once", s),
        }
    }
}

#[derive(Clone, Debug)]
pub enum KindError {
    NameError(String),
    ArgMismatch(Kind, Kind),
    FuncApp,
}

impl fmt::Display for KindError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            KindError::NameError(ref s) => {
                write!(f, "type eval error: variable {} not found", s)
            }
            KindError::ArgMismatch(ref expected, ref actual) => write!(
                f,
                "Expected argument of kind {} but got {} instead",
                expected, actual
            ),
            KindError::FuncApp => write!(f, "Tried to apply non function kind"),
        }
    }
}
