#![feature(box_patterns)]
#![feature(const_fn)]
#![feature(const_vec_new)]

extern crate lalrpop_util;
extern crate wasm_bindgen;

pub mod assoclist;
pub mod errors;
pub mod eval;
pub mod grammar;
pub mod kindcheck;
pub mod program;
pub mod syntax;
pub mod test_grammar;
pub mod typecheck;
pub mod wasm;

pub use wasm::PROGRAM;
pub use wasm::*;
