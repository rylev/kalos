use llvm_sys::core::{LLVMFunctionType,LLVMDoubleType};
use llvm_sys::prelude::{LLVMTypeRef,LLVMBool};
use std::os::raw::c_uint;

#[derive(PartialEq)]
pub enum ArgsLengthType {
    VariableLength,
    FixedLength
}

impl ArgsLengthType {
    fn is_variable(&self) -> bool {
        match *self {
            ArgsLengthType::VariableLength => true,
            _                              => false
        }
    }
}

pub trait Type {
    fn to_ref(&self) -> LLVMTypeRef;
}

pub struct FunctionType {
    reference: LLVMTypeRef
}

impl FunctionType {
    pub fn new<T>(param_types: &mut Vec<T>, return_type: &T, args_length_type: ArgsLengthType) -> Self where T: Type {
        let reference = unsafe {
            LLVMFunctionType(
                return_type.to_ref(),
                param_types.as_mut_slice().iter().map(|ref t| t.to_ref()).collect::<Vec<LLVMTypeRef>>().as_mut_ptr(),
                param_types.len() as c_uint,
                args_length_type.is_variable() as LLVMBool
            )
        };

        Self::from_ref(reference)
    }

    pub fn from_ref(reference: LLVMTypeRef) -> Self {
        FunctionType { reference: reference }
    }
}


impl Type for FunctionType {
    fn to_ref(&self) -> LLVMTypeRef {
        self.reference
    }
}

#[derive(Copy, Clone)]
pub struct RealType {
    reference: LLVMTypeRef
}

impl RealType {
    pub fn from_ref(reference: LLVMTypeRef) -> Self {
        RealType { reference: reference }
    }

    pub fn double() -> Self {
        let reference  = unsafe { LLVMDoubleType() };
        Self::from_ref(reference)
    }
}

impl Type for RealType {
    fn to_ref(&self) -> LLVMTypeRef {
        self.reference
    }
}

