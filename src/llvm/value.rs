use std::ffi::CString;
use std::os::raw::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::LLVMValueRef;

use super::module::Module;
use super::basic_block::BasicBlock;
use super::context::Context;
use super::llvm_type::{FunctionType,RealType};
use super::llvm_type::Type;

pub struct Function {
    reference: LLVMValueRef
}

impl Function {
    pub fn new(module: &mut Module, name: &str, typ: &FunctionType) -> Self {
        let name = CString::new(name).unwrap();
        let reference = unsafe { LLVMAddFunction(module.to_ref(), name.as_ptr() as *const c_char, typ.to_ref()) };

        Self::from_ref(reference)
    }

    pub fn to_ref(&self) -> LLVMValueRef {
        self.reference
    }

    pub fn from_ref(reference: LLVMValueRef) -> Self {
        Function { reference: reference }
    }

    pub fn append_basic_block_in_context(&mut self, context: &mut Context, name: &str) -> BasicBlock {
        let name = CString::new(name).unwrap();
        let reference = unsafe {
            LLVMAppendBasicBlockInContext(context.to_ref(), self.to_ref(), name.as_ptr() as *const c_char)
        };
        BasicBlock::from_ref(reference)
    }
}

pub struct RealConst {
    reference: LLVMValueRef
}

impl RealConst {
    pub fn new(typ: &RealType, value: f64) -> Self {
        let reference = unsafe { LLVMConstReal(typ.to_ref(), value) };
        Self::from_ref(reference)
    }

    pub fn from_ref(reference: LLVMValueRef) -> Self {
        RealConst { reference: reference }
    }

    pub fn to_ref(&self) -> LLVMValueRef {
        self.reference
    }
}

