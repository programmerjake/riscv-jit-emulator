// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::backend;
use alloc::vec::Vec;
use core::{cell::UnsafeCell, fmt, mem::ManuallyDrop};

mod wrappers;

use wrappers::{LlvmContext, Own, Ref};

struct CompilerData {
    /// `execution_engine` must be dropped before `contexts`
    execution_engine: ManuallyDrop<Own<wrappers::LlvmExecutionEngine>>,
    /// only used on a single thread before compilation, not accessed after compilation,
    /// just to keep the contexts live since LLVM uses them internally.
    contexts: UnsafeCell<Vec<Own<wrappers::LlvmContext>>>,
}

impl Drop for CompilerData {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.execution_engine) }
    }
}

impl fmt::Debug for CompilerData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CompilerData {{ ... }}")
    }
}

#[derive(Debug)]
pub struct CompiledCode {
    compiler_data: CompilerData,
}

unsafe impl Send for CompiledCode {}

unsafe impl Sync for CompiledCode {}

impl backend::CompiledCode for CompiledCode {}

#[derive(Debug, Copy, Clone)]
pub struct TypeRef<'compiler, 'ctx>(Ref<'ctx, wrappers::LlvmType<'compiler>>);

impl<'compiler, 'ctx> backend::TypeRef for TypeRef<'compiler, 'ctx> {}

#[derive(Debug, Clone, Copy)]
pub struct ContextRef<'compiler, 'ctx> {
    context: Ref<'ctx, wrappers::LlvmContext>,
    code: &'compiler CompilerData,
}

impl<'compiler, 'ctx> backend::ContextRef for ContextRef<'compiler, 'ctx> {
    type Type = TypeRef<'compiler, 'ctx>;

    type BoolType = TypeRef<'compiler, 'ctx>;

    fn bool_type(self) -> Self::BoolType {
        todo!()
    }

    type F32Type = TypeRef<'compiler, 'ctx>;

    fn f32_type(self) -> Self::F32Type {
        todo!()
    }

    type F64Type = TypeRef<'compiler, 'ctx>;

    fn f64_type(self) -> Self::F64Type {
        todo!()
    }

    type I8Type = TypeRef<'compiler, 'ctx>;

    fn i8_type(self) -> Self::I8Type {
        todo!()
    }

    type I16Type = TypeRef<'compiler, 'ctx>;

    fn i16_type(self) -> Self::I16Type {
        todo!()
    }

    type I32Type = TypeRef<'compiler, 'ctx>;

    fn i32_type(self) -> Self::I32Type {
        todo!()
    }

    type I64Type = TypeRef<'compiler, 'ctx>;

    fn i64_type(self) -> Self::I64Type {
        todo!()
    }

    type I128Type = TypeRef<'compiler, 'ctx>;

    fn i128_type(self) -> Self::I128Type {
        todo!()
    }

    type ISizeType = TypeRef<'compiler, 'ctx>;

    fn isize_type(self) -> Self::ISizeType {
        todo!()
    }
}

#[derive(Copy, Clone, Debug)]
pub struct BackendImpl;

impl backend::Backend for BackendImpl {
    type CompiledCode = CompiledCode;

    fn with_compiler<F: backend::CallWithCompiler>(
        &self,
        f: F,
    ) -> Result<Self::CompiledCode, F::Error> {
        let context = LlvmContext::new();
        let compiler_data = CompilerData {
            execution_engine: todo!(),
            contexts: UnsafeCell::new(Vec::new()),
        };
        todo!()
    }
}
