use llvm_sys::target_machine::*;
use llvm_sys::target::*;
use super::module::Module;

use std::ops::Drop;
use std::os::raw::c_char;
use std::ffi::CStr;
use std::ffi::CString;
use std::mem;

#[derive(Debug)]
pub struct TargetMachine{
    reference: LLVMTargetMachineRef
}

#[derive(Copy, Clone, Debug)]
pub enum TargetMachineError {
    CouldntInitNativeTarget,
    CouldntInitNativeAsmPrinter,
    // CouldntGetTargetFromTriple,
}

fn inititalize_native_target() -> bool {
    unsafe { LLVM_InitializeNativeTarget() == 0 }
}

fn initialize_native_asm_printer() -> bool {
    unsafe { LLVM_InitializeNativeAsmPrinter() == 0 }
}

fn get_default_target_triple<'a>() -> &'a str {
    let cstring = unsafe { CStr::from_ptr(LLVMGetDefaultTargetTriple()) };
    cstring.to_str().expect("Could not turn c string into rust str")
}

fn get_target_from_triple(triple: &str) -> LLVMTargetRef {
    // TODO: handle error
    unsafe {

        let mut target = mem::uninitialized();
        let mut error = mem::uninitialized();
        LLVMGetTargetFromTriple(CString::new(triple).unwrap().as_ptr() as *const i8, &mut target, error);

        target
    }
}

fn cstring(string: &str) -> *const c_char {
    CString::new(string).unwrap().as_ptr() as *const c_char
}

impl TargetMachine {
    pub fn new() -> Result<Self, TargetMachineError> {
        if !inititalize_native_target() {
            return Err(TargetMachineError::CouldntInitNativeTarget);
        }
        if !initialize_native_asm_printer() {
            return Err(TargetMachineError::CouldntInitNativeAsmPrinter);
        }

        let triple = get_default_target_triple();
        let target = get_target_from_triple(triple);

        let reference = unsafe {
            LLVMCreateTargetMachine(
                target,
                cstring(triple),
                cstring(""),
                cstring(""),
                LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault, //TODO: allow this to be passed in
                LLVMRelocMode::LLVMRelocDefault,
                LLVMCodeModel::LLVMCodeModelDefault
            )
        };

        let target_machine = TargetMachine { reference: reference };
        Ok(target_machine)
    }

    pub fn emit_to_file(&self, module: &Module, path: &str) -> Result<(), String> {
        let path = CString::new(path).unwrap();
        unsafe {
            let mut error = mem::uninitialized();
            if LLVMTargetMachineEmitToFile(self.reference, module.to_ref(), path.as_ptr() as *mut c_char, LLVMCodeGenFileType::LLVMObjectFile, &mut error) == 0 {
                Ok(())
            } else {
                Err(CStr::from_ptr(error).to_string_lossy().into_owned())
            }
        }
    }
}

impl Drop for TargetMachine {
    fn drop(&mut self) {
        unsafe { LLVMDisposeTargetMachine(self.reference) }
    }
}
