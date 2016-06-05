#[macro_use]
extern crate nom;
extern crate llvm_sys;
mod lexer;
mod parser;
mod ast;
mod llvm;

pub use parser::parse;

use std::iter::repeat;

use llvm_sys::prelude::LLVMValueRef;

use llvm::*;

#[derive(Debug)]
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
        let mut function = match module.get_function_by_name(&self.name()) {
            Some(_) => return Err(CodeGenError::Wtf), // handle redefinition
            None => {
                // function type is defined by number and types of the arguments
                let float_type = RealType::double();
                let mut param_types : Vec<RealType> = repeat(float_type).take(self.args().len()).collect();
                let function_type = FunctionType::new(&mut param_types, &float_type, false);
                Function::new(module, &self.name(), &function_type)
            }
        };

        //TODO: set param names
        let mut basic_block = function.append_basic_block_in_context(&mut context_provider.context, "entry");
        context_provider.builder.position_at_end(&mut basic_block);

        // TODO: Set function params in named values
        let end_expression = self.body().last().unwrap();
        let end = try!(end_expression.codegen(context_provider, module));

        context_provider.builder.build_ret(end);

        Ok(function.to_ref())
    }
}

impl<'a> IRBuilder for ast::Expression<'a> {
    fn codegen(&self, _context_provider: &mut ContextProvider, _module: &mut Module) -> IRResult {
        match self {
            &ast::Expression::Value(value) => {
                let typ = RealType::double();
                Ok(RealConst::new(&typ, value).to_ref())
            }
            _ => panic!("Unknown expression")
        }
    }
}

#[test]
fn foo() {
    let function = ast::Function::new("foo", vec!("bar", "baz"), ast::Expression::Value(1.0));
    let mut context_provider = ContextProvider::new();
    let mut module = Module::new();
    let result = function.codegen(&mut context_provider, &mut module);
    println!("{:?}", result);
    module.dump();
    panic!("")
}
