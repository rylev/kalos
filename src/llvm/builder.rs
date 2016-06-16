use std::ffi::CString;
use std::os::raw::{c_char,c_uint};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm::basic_block::BasicBlock;

pub struct Builder {
    reference: LLVMBuilderRef
}

impl Builder {
    pub fn new() -> Builder {
        let reference = unsafe { LLVMCreateBuilder() };

        Builder {
            reference: reference
        }
    }

    pub fn position_at_end(&mut self, block: &mut BasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.to_ref(), block.to_ref()) }
    }


    pub fn build_fadd(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFAdd(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }


    pub fn build_fsub(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFSub(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_fmul(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFMul(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_fdiv(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFDiv(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_neg(&mut self, value: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNeg(self.to_ref(), value, name.as_ptr() as *const c_char) }
    }

    pub fn build_call(&mut self, func: LLVMValueRef, args: &mut [LLVMValueRef], name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildCall(
                self.to_ref(),
                func,
                args.as_mut_ptr(),
                args.len() as c_uint,
                name.as_ptr() as *const c_char
            )
        }
    }

    pub fn build_ret(&self, value: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildRet(self.to_ref(), value) }
    }

    fn to_ref(&self) -> LLVMBuilderRef {
        self.reference
    }
}
