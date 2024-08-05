use crate::ast;

use llvm_sys::prelude::{LLVMBuilderRef, LLVMModuleRef, LLVMValueRef};
use std::ffi::{CStr, CString};

use llvm_sys::prelude::LLVMContextRef;
use llvm_sys::core::{LLVMAddFunction, LLVMAppendBasicBlock, LLVMBuildRet, LLVMConstInt, LLVMContextCreate, LLVMContextDispose, LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMDisposeMessage, LLVMDisposeModule, LLVMFunctionType, LLVMInt32TypeInContext, LLVMModuleCreateWithNameInContext, LLVMPositionBuilderAtEnd, LLVMPrintModuleToString, LLVMVoidTypeInContext};

#[allow(dead_code)]
struct LLVMCodeGen {
    ctx: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
}

impl LLVMCodeGen {
    pub fn new(name: &str) -> Self {
        let name = CString::new(name).unwrap();
        unsafe {
            let ctx = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(
                name.as_ptr(), ctx
            );
            let builder = LLVMCreateBuilderInContext(ctx);

            LLVMCodeGen {
                ctx,
                module,
                builder,
            }
        }
    }
}

impl Drop for LLVMCodeGen {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.ctx);
        }
    }
}


impl<'a> ast::Program {
    pub fn to_llvm(self, name: &str) -> String {
        let codegen = LLVMCodeGen::new(name);
        unsafe {
            self.body.to_llvm(&codegen);
            let code = LLVMPrintModuleToString(codegen.module);
            let result = CStr::from_ptr(code).to_string_lossy().into_owned();
            LLVMDisposeMessage(code as *mut _);
            result
        }
    }
}

impl<'a> ast::Function {
    unsafe fn to_llvm(self, llvm: &LLVMCodeGen) -> LLVMValueRef {
        let name = CString::new(self.name).unwrap();
        let fn_type = LLVMInt32TypeInContext(llvm.ctx);
        let param_types = [LLVMVoidTypeInContext(llvm.ctx)].as_mut_ptr();
        let fn_type = LLVMFunctionType(
            fn_type, param_types, 0, 0
        );
        let func = LLVMAddFunction(
            llvm.module, name.as_ptr(), fn_type
        );
        let block_name = CString::new("entry").unwrap();
        let block = LLVMAppendBasicBlock(func, block_name.as_ptr());
        LLVMPositionBuilderAtEnd(llvm.builder, block);
        self.body.to_llvm(llvm);
        func
    }
}

impl<'a> ast::Statement {
    unsafe fn to_llvm(self, llvm: &LLVMCodeGen) {
        match self {
            ast::Statement::Return(expr) => {
                let value = expr.to_llvm(llvm);
                LLVMBuildRet(llvm.builder, value);
            }
        }
    }
}

impl<'a> ast::Expression {
    unsafe fn to_llvm(self, llvm: &LLVMCodeGen) -> LLVMValueRef {
        match self {
            ast::Expression::Constant(ref val) => {
                LLVMConstInt(
                    LLVMInt32TypeInContext(llvm.ctx), *val as u64, 0
                )
            },
            // ast::Expression::Unary(op, expr) => {
            //     todo!()
            // },
            // ast::Expression::Binary(
            //     op, left, right
            // ) => {
            //     todo!()
            // },
            _ => todo!()
        }
    }
}
