#[macro_use]
extern crate nom;
extern crate llvm_sys as llvm;
mod lexer;
mod parser;

pub use parser::parse;
use llvm::*;
use llvm::core::*;
use llvm::prelude::*;
use llvm::target::*;
use llvm::target_machine::*;
use llvm::transforms::scalar::*;
use llvm::analysis::*;

macro_rules! c_str {
    ($s:expr) => {{
        concat!($s, "\0").as_ptr() as *const i8
    }}
}

pub struct Type{
    reference: LLVMTypeRef
}

pub struct Module {
    reference: LLVMModuleRef
}

impl Module {
  pub fn new(name: &str) -> Self {
    unsafe {
      Module { reference: LLVMModuleCreateWithName(c_str!("hello")) }
    }
  }
}

impl std::ops::Drop for Module {
  fn drop(&mut self) {
    unsafe { LLVMDisposeModule(self.reference); }
  }
}

#[derive(Debug)]
pub struct Value{
    reference: LLVMValueRef
}

impl Value {
    pub fn new(reference: LLVMValueRef) -> Value {
        Value { reference: reference }
    }

    pub fn const_fp(ty: Type, value: f64) -> Value {
        unsafe {
            Value::new(LLVMConstReal(ty.reference, value))
        }
    }
}
