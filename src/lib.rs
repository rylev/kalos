#[macro_use]
extern crate nom;
extern crate llvm_sys as llvm;
mod lexer;
mod parser;
mod ast;

pub use parser::parse;
use llvm::core::*;
use llvm::prelude::*;
use std::collections::HashMap;
use std::ffi::CString;
use std::iter;
use std::os::raw::c_uint;
use std::os::raw::c_char;

pub enum CodeGenError {
    Wtf
}
pub type IRResult = Result<LLVMValueRef, CodeGenError>;
pub trait IRBuilder {
    fn codegen(&self, context_provider: &mut ContextProvider, module: &mut Module) -> IRResult;
}

impl<'a> IRBuilder for ast::Ast<'a> {
    fn codegen(&self, context_provider: &mut ContextProvider, module: &mut Module) -> IRResult {
        let mut result = Err(CodeGenError::Wtf);
        for node in self.nodes() {
            result = Ok(try!(node.codegen(context_provider, module)))
        }
        result
    }
}

impl<'a> IRBuilder for ast::AstNode<'a> {
    fn codegen(&self, context_provider: &mut ContextProvider, module: &mut Module) -> IRResult {
        match self {
            &ast::AstNode::FuncDeclartion(ref func) => func.codegen(context_provider, module)
        }
    }
}

impl<'a> IRBuilder for ast::Function<'a> {
    fn codegen(&self, context_provider: &mut ContextProvider, module: &mut Module) -> IRResult {
        let function = match module.get_function_by_name(&self.name()) {
            Some(func) => return Err(CodeGenError::Wtf), // handle redefinition
            None => {
                // function type is defined by number and types of the arguments
                let float_type = RealType::double();
                let mut param_types : Vec<RealType> = iter::repeat(float_type).take(self.args().len()).collect();
                let function_type = FunctionType::new(&mut param_types, &float_type, false);
                Function::new(module, &self.name(), &function_type)
            }
        };

        //TODO: set param names
        (Ok(function.to_ref()))
    }
}

macro_rules! c_str {
    ($s:expr) => {{
        concat!($s, "\0").as_ptr() as *const i8
    }}
}

pub struct ContextProvider {
    context: Context,
    builder: Builder,
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
}

impl Drop for Context {
    fn drop(&mut self) {
       if !self.global {
            unsafe { LLVMContextDispose(self.reference); }
        }
    }
}


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

    fn get_function_by_name(&self, name: &str) -> Option<Function> {
        let name = CString::new(name).unwrap();
        let function = unsafe {
            LLVMGetNamedFunction(self.reference, name.as_ptr() as *const i8)
        };
        if !function.is_null() {
            unsafe { Some(Function::from_ref(function)) }
        } else {
            None
        }
    }

    fn to_ref(&self) -> LLVMModuleRef {
        self.reference
    }

    // Print module to stdout
    pub fn dump(&self) {
        unsafe { LLVMDumpModule(self.reference) }
    }
}

impl std::ops::Drop for Module {
    fn drop(&mut self) {
        unsafe { LLVMDisposeModule(self.reference); }
    }
}

struct Function {
    reference: LLVMValueRef
}

impl Function {
    fn new(module: &mut Module, name: &str, typ: &FunctionType) -> Self {
        let name = CString::new(name).unwrap();
        let reference = unsafe {
            LLVMAddFunction(module.to_ref(), name.as_ptr() as *const c_char, typ.to_ref())
        };

        Self::from_ref(reference)
    }

    fn from_ref(reference: LLVMValueRef) -> Self {
        Function { reference: reference }
    }

    fn to_ref(&self) -> LLVMValueRef {
        self.reference
    }
}

// Types

trait Type {
    fn to_ref(&self) -> LLVMTypeRef;
}

struct FunctionType {
    reference: LLVMTypeRef
}

impl FunctionType {
    fn from_ref(reference: LLVMTypeRef) -> Self {
        FunctionType { reference: reference }
    }

    fn new<T>(param_types: &mut Vec<T>, return_type: &T, is_var_arg: bool) -> Self where T: Type {
        let reference = unsafe {
            LLVMFunctionType(
                return_type.to_ref(),
                param_types.as_mut_slice().iter().map(|ref t| t.to_ref()).collect::<Vec<LLVMTypeRef>>().as_mut_ptr(),
                param_types.len() as c_uint,
                is_var_arg as LLVMBool
            )
        };

        Self::from_ref(reference)
    }
}

impl Type for FunctionType {
    fn to_ref(&self) -> LLVMTypeRef {
        self.reference
    }
}

#[derive(Copy, Clone)]
struct RealType {
    reference: LLVMTypeRef
}

impl RealType {
    fn from_ref(reference: LLVMTypeRef) -> Self {
        RealType { reference: reference }
    }

    fn double() -> Self {
        let reference  = unsafe { LLVMDoubleType() };
        Self::from_ref(reference)
    }
}

impl Type for RealType {
    fn to_ref(&self) -> LLVMTypeRef {
        self.reference
    }
}
