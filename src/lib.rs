#[macro_use]
extern crate nom;
extern crate llvm_sys as llvm;
mod lexer;
mod parser;

pub use parser::parse;
use llvm::*;
use self::llvm_sys::core::*;

pub struct Module {
    reference: LLVMModuleRef
}


macro_rules! cstr {
    ($s:expr) => (concat!($s, "\0").as_ptr() as *const self::libc::c_char)
}


impl Module {
  pub fn new(name: &str) -> Self {
    unsafe {
      Module(LLVMModuleCreateWithName(cstr!("")))
    }
  }

  pub fn add_function(&self, name: &str, typ: Type) -> Value {
    unsafe {
      Value::new(LLVMAddFunction(self.reference, CString::new(name.to_owned()).unwrap().as_ptr(), ty.0))
    }
  }

  pub fn dump(&self) {
    unsafe { LLVMDumpModule(self.reference) }
  }

  pub fn verify(&self) {
    unsafe {
      let mut error: *mut c_char = std::mem::uninitialized();
      LLVMVerifyModule(self.reference, LLVMVerifierFailureAction::LLVMAbortProcessAction, &mut error);
      LLVMDisposeMessage(error);
    }
  }
}

impl std::ops::Drop for Module {
  fn drop(&mut self) {
    unsafe { LLVMDisposeModule(self.0); }
  }
}

#[derive(Copy, Clone, Debug)]
pub struct Value{
    reference: LLVMValueRef
}

impl Value {
    pub fn new(reference: LLVMValueRef) -> Value {
        Value { reference: reference }
    }

    pub fn const_int(ty: Type, value: f64) -> Value {
        unsafe {
            Value(LLVMConstReal(ty.reference, value))
        }
    }

    #[allow(dead_code)]
    pub fn dump(self) {
        unsafe {
            LLVMDumpValue(self.0)
        }
    }
}

