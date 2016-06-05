use llvm_sys::prelude::LLVMBasicBlockRef;

pub struct BasicBlock {
    reference: LLVMBasicBlockRef
}

impl BasicBlock {
    pub fn from_ref(reference: LLVMBasicBlockRef) -> Self {
        BasicBlock { reference: reference }
    }

    pub fn to_ref(&self) -> LLVMBasicBlockRef {
        self.reference
    }
}
