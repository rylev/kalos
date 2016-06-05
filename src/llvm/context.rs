use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm::builder::Builder;
use std::collections::HashMap;

#[allow(dead_code)]
pub struct ContextProvider {
    pub context: Context,
    pub builder: Builder,
    named_values: HashMap<String, LLVMValueRef>
}

impl ContextProvider {
    pub fn new() -> ContextProvider {
        let context = Context::global();
        let builder = Builder::new();
        let named_values = HashMap::new();

        ContextProvider {
            context: context,
            builder: builder,
            named_values: named_values,
        }
    }
}

pub struct Context {
    reference: LLVMContextRef,
    global: bool
}

impl Context {
    pub fn new() -> Context {
        let reference = unsafe { LLVMContextCreate() };

        Context{
            reference: reference,
            global: false
        }
    }

    pub fn global() -> Context {
        let reference = unsafe { LLVMGetGlobalContext() };

        Context {
            reference: reference,
            global: true
        }
    }

    pub fn to_ref(&self) -> LLVMContextRef {
        self.reference
    }
}

impl Drop for Context {
    fn drop(&mut self) {
       if !self.global {
            unsafe { LLVMContextDispose(self.reference); }
        }
    }
}
