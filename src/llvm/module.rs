use std::ffi::CString;
use std::ops::Drop;

use llvm_sys::core::*;
use llvm_sys::prelude::LLVMModuleRef;

use super::value::Function;

pub struct Module {
    reference: LLVMModuleRef
}

impl Module {
    pub fn new(name: &str) -> Self {
        let name = CString::new(name).unwrap();
        unsafe {
            Module { reference: LLVMModuleCreateWithName(name.as_ptr() as *const i8) }
        }
    }

    pub fn get_function_by_name(&self, name: &str) -> Option<Function> {
        let name = CString::new(name).unwrap();
        let function = unsafe {
            LLVMGetNamedFunction(self.reference, name.as_ptr() as *const i8)
        };
        if !function.is_null() {
            Some(Function::from_ref(function))
        } else {
            None
        }
    }

    pub fn to_ref(&self) -> LLVMModuleRef {
        self.reference
    }

    // Print module to stdout
    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.reference) }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { LLVMDisposeModule(self.reference); }
    }
}
