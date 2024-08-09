use crate::ast;

use llvm_sys::prelude::{LLVMBuilderRef, LLVMModuleRef, LLVMValueRef};
use llvm_sys::LLVMValue;
use std::ffi::{CStr, CString};

use llvm_sys::core::{
    LLVMAddFunction, LLVMAppendBasicBlock, LLVMBuildAdd, LLVMBuildMul, LLVMBuildNeg, LLVMBuildNot,
    LLVMBuildRet, LLVMBuildSDiv, LLVMBuildSRem, LLVMBuildSub, LLVMConstInt, LLVMContextCreate,
    LLVMContextDispose, LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMDisposeMessage,
    LLVMDisposeModule, LLVMFunctionType, LLVMInt32TypeInContext, LLVMModuleCreateWithNameInContext,
    LLVMPositionBuilderAtEnd, LLVMPrintModuleToString, LLVMVoidTypeInContext,
};
use llvm_sys::prelude::LLVMContextRef;

#[allow(dead_code)]
struct LLIrCtx {
    ll: LLVMContextRef,
    llmod: LLVMModuleRef,
    builder: LLVMBuilderRef,
    temp_var_id: u64,
}

impl LLIrCtx {
    pub fn new(name: &str) -> Self {
        let name = CString::new(name).unwrap();
        unsafe {
            let ctx = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(name.as_ptr(), ctx);
            let builder = LLVMCreateBuilderInContext(ctx);

            LLIrCtx {
                ll: ctx,
                llmod: module,
                builder,
                temp_var_id: 0,
            }
        }
    }

    pub fn temp_var(&mut self) -> CString {
        let id = self.temp_var_id;
        self.temp_var_id += 1;
        CString::new(format!("tmp.{}", id)).unwrap()
    }
}

impl Drop for LLIrCtx {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.llmod);
            LLVMContextDispose(self.ll);
        }
    }
}

impl<'a> ast::Program {
    pub fn to_llvm(self, name: &str) -> String {
        let mut codegen = LLIrCtx::new(name);
        unsafe {
            self.body.to_llvm(&mut codegen);
            let code = LLVMPrintModuleToString(codegen.llmod);
            let result = CStr::from_ptr(code).to_string_lossy().into_owned();
            LLVMDisposeMessage(code as *mut _);
            result
        }
    }
}

impl<'a> ast::Function {
    unsafe fn to_llvm(self, llvm: &mut LLIrCtx) -> LLVMValueRef {
        let name = CString::new(self.name).unwrap();
        let fn_type = LLVMInt32TypeInContext(llvm.ll);
        let param_types = [LLVMVoidTypeInContext(llvm.ll)].as_mut_ptr();
        let fn_type = LLVMFunctionType(fn_type, param_types, 0, 0);
        let func = LLVMAddFunction(llvm.llmod, name.as_ptr(), fn_type);
        let block_name = CString::new("entry").unwrap();
        let block = LLVMAppendBasicBlock(func, block_name.as_ptr());
        LLVMPositionBuilderAtEnd(llvm.builder, block);
        self.body.to_llvm(llvm);
        func
    }
}

impl<'a> ast::Statement {
    unsafe fn to_llvm(self, llvm: &mut LLIrCtx) {
        match self {
            ast::Statement::Return(expr) => {
                let value = expr.to_llvm(llvm);
                LLVMBuildRet(llvm.builder, value);
            }
        }
    }
}

impl<'a> ast::Expression {
    unsafe fn to_llvm(self, llvm: &mut LLIrCtx) -> LLVMValueRef {
        match self {
            ast::Expression::Constant(ref val) => {
                LLVMConstInt(LLVMInt32TypeInContext(llvm.ll), *val as u64, 0)
            }
            ast::Expression::Unary(op, expr) => {
                let op = op.to_llvm();
                let val = expr.to_llvm(llvm);
                let name = CString::new("negtmp").unwrap();
                op(llvm.builder, val, name.as_ptr())
            }
            ast::Expression::Binary(op, left, right) => {
                let op = op.to_llvm();
                let left = left.to_llvm(llvm);
                let right = right.to_llvm(llvm);
                let dst = llvm.temp_var();
                op(llvm.builder, left, right, dst.as_ptr())
            }
        }
    }
}

type LLVMUnaryOpFn =
    unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, *const i8) -> *mut LLVMValue;

impl<'a> ast::UnaryOperator {
    unsafe fn to_llvm(self) -> LLVMUnaryOpFn {
        match self {
            ast::UnaryOperator::Negation => LLVMBuildNeg,
            ast::UnaryOperator::Complement => LLVMBuildNot,
            // _ => todo!(),
        }
    }
}

type LLVMBinaryOpFn =
    unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const i8) -> *mut LLVMValue;

impl<'a> ast::BinaryOperator {
    unsafe fn to_llvm(self) -> LLVMBinaryOpFn {
        match self {
            ast::BinaryOperator::Add => LLVMBuildAdd,
            ast::BinaryOperator::Subtract => LLVMBuildSub,
            ast::BinaryOperator::Multiply => LLVMBuildMul,
            ast::BinaryOperator::Divide => LLVMBuildSDiv,
            ast::BinaryOperator::Remainder => LLVMBuildSRem,
            // _ => todo!(),
        }
    }
}
