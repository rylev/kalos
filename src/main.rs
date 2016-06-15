#[macro_use]
extern crate nom;
extern crate llvm_sys;
mod lexer;
mod parser;
mod ast;
mod llvm;

use std::iter::repeat;

use llvm_sys::prelude::LLVMValueRef;

use llvm::*;

#[derive(Debug)]
pub enum CodeGenError {
    UnresolvedName(String),
    AlreadyDefinedFunction(String),
    Wtf //TODO: get rid of this "catch all" error
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
        // Start with a fresh context
        context_provider.named_values.clear();

        let mut function_value = match module.get_function_by_name(&self.name()) {
            Some(_) => return Err(CodeGenError::AlreadyDefinedFunction(self.name().to_owned())),
            None => {
                // function type is defined by number and types of the arguments
                let float_type = RealType::double();
                let mut param_types : Vec<RealType> = repeat(float_type).take(self.args().len()).collect();
                let return_type = float_type;
                let function_type = FunctionType::new(&mut param_types, &return_type, ArgsLengthType::FixedLength);
                Function::new(module, &self.name(), &function_type)
            }
        };

        for (&arg, parameter) in self.args().iter().zip(function_value.params()) {

            parameter.set_name(arg);
            context_provider.named_values.insert(arg.to_owned(), parameter.to_ref());
        }
        let mut basic_block = function_value.append_basic_block_in_context(&mut context_provider.context, "entry");
        context_provider.builder.position_at_end(&mut basic_block);

        // TODO: Set function params in named values
        let end_expression = self.body().last().unwrap();
        let end = try!(end_expression.codegen(context_provider, module));

        context_provider.builder.build_ret(end);

        // Variables local to the function are cleared
        context_provider.named_values.clear();

        Ok(function_value.to_ref())
    }
}

impl<'a> IRBuilder for ast::Expression<'a> {
    fn codegen(&self, context_provider: &mut ContextProvider, module: &mut Module) -> IRResult {
        match self {
            &ast::Expression::Value(value) => {
                let typ = RealType::double();
                Ok(RealConst::new(&typ, value).to_ref())
            }
            &ast::Expression::Add(ref lhs, ref rhs) => {
                let lhs = try!(lhs.codegen(context_provider, module));
                let rhs = try!(rhs.codegen(context_provider, module));
                Ok(context_provider.builder.build_fadd(lhs, rhs, "addtmp"))
            }
            &ast::Expression::Subtract(ref lhs, ref rhs) => {
                let lhs = try!(lhs.codegen(context_provider, module));
                let rhs = try!(rhs.codegen(context_provider, module));
                Ok(context_provider.builder.build_fsub(lhs, rhs, "subtmp"))
            }
            &ast::Expression::Variable(name) => {
                match context_provider.named_values.get(name) {
                    Some(value) => Ok(*value),
                    None        => Err(CodeGenError::UnresolvedName(name.to_owned()))
                }
            }
            _ => panic!("Unknown expression")
        }
    }
}

fn main() {
    let ast = parser::parse("def foo(boo, baz) { boo + 1 - 1 }").unwrap();
    let mut context_provider = ContextProvider::new();
    let mut module = Module::new();
    let _ = ast.codegen(&mut context_provider, &mut module).unwrap();
    module.dump();
}
