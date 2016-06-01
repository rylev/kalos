#[macro_use]
extern crate nom;
mod lexer;
mod parser;

pub use parser::parse;
