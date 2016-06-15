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

    pub fn params(&self) -> ParamsIter {
        ParamsIter::new(self)
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

pub struct Parameter {
    reference: LLVMValueRef
}

impl Parameter {
    pub fn from_ref(reference: LLVMValueRef) -> Self {
        Parameter { reference: reference }
    }

    pub fn to_ref(&self) -> LLVMValueRef {
        self.reference
    }

    pub fn set_name(&self, name: &str) {
        let name = CString::new(name).unwrap();
        unsafe { LLVMSetValueName(self.to_ref(), name.as_ptr() as *const i8) }
    }
}

pub struct ParamsIter {
    next: Option<Parameter>,
    pos: u32,
    size: u32
}

impl ParamsIter {
    fn new(function: &Function) -> ParamsIter {
        let size = unsafe { LLVMCountParams(function.to_ref()) };
        let first_param = if size > 0 {
            Some(Parameter::from_ref(unsafe { LLVMGetFirstParam(function.to_ref()) }))
        } else {
            None
        };

        ParamsIter {
            next: first_param,
            pos: 0,
            size: size
        }
    }
}

impl Iterator for ParamsIter {
    type Item = Parameter;

    fn next(&mut self) -> Option<Parameter> {
        let (current, next) = if let Some(ref parameter) = self.next {
            let current = Some(Parameter::from_ref(parameter.to_ref()));
            let next = if self.pos < self.size - 1 {
                Some(Parameter::from_ref(unsafe { LLVMGetNextParam(parameter.to_ref()) }))
            }  else {
                None
            };
            (current, next)
        } else {
            (None, None)
        };

        if current.is_some() {
            self.pos += 1;
        }

        self.next = next;

        current
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let number_remaining_parameters = (self.size - self.pos) as usize;
        (number_remaining_parameters, Some(number_remaining_parameters))
    }
}
