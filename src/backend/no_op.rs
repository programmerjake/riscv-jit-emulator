// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::backend;
use core::marker::PhantomData;

#[derive(Debug)]
struct Code(());

impl Drop for Code {
    fn drop(&mut self) {}
}

#[derive(Debug)]
pub struct CompiledCode(Code);

impl backend::CompiledCode for CompiledCode {}

#[derive(Debug, Clone, Copy)]
pub struct CompilerRef<'compiler>(&'compiler Code);

impl<'compiler> backend::CompilerRef for CompilerRef<'compiler> {
    fn with_context<F: backend::CallWithContext>(self, f: F) -> F::Output {
        let context = Context(self);
        f.call_with_context(ContextRef(&context))
    }
}

#[derive(Debug)]
struct Context<'compiler>(CompilerRef<'compiler>);

#[derive(Debug, Clone, Copy)]
pub struct ContextRef<'compiler, 'ctx>(&'ctx Context<'compiler>);

impl<'compiler, 'ctx> backend::ContextRef for ContextRef<'compiler, 'ctx> {
    type Type = TypeRef<'compiler, 'ctx>;

    type BoolType = TypeRef<'compiler, 'ctx>;

    fn bool_type(self) -> Self::BoolType {
        TypeRef(PhantomData)
    }

    type F32Type = TypeRef<'compiler, 'ctx>;

    fn f32_type(self) -> Self::F32Type {
        TypeRef(PhantomData)
    }

    type F64Type = TypeRef<'compiler, 'ctx>;

    fn f64_type(self) -> Self::F64Type {
        TypeRef(PhantomData)
    }

    type I8Type = TypeRef<'compiler, 'ctx>;

    fn i8_type(self) -> Self::I8Type {
        TypeRef(PhantomData)
    }

    type I16Type = TypeRef<'compiler, 'ctx>;

    fn i16_type(self) -> Self::I16Type {
        TypeRef(PhantomData)
    }

    type I32Type = TypeRef<'compiler, 'ctx>;

    fn i32_type(self) -> Self::I32Type {
        TypeRef(PhantomData)
    }

    type I64Type = TypeRef<'compiler, 'ctx>;

    fn i64_type(self) -> Self::I64Type {
        TypeRef(PhantomData)
    }

    type I128Type = TypeRef<'compiler, 'ctx>;

    fn i128_type(self) -> Self::I128Type {
        TypeRef(PhantomData)
    }

    type ISizeType = TypeRef<'compiler, 'ctx>;

    fn isize_type(self) -> Self::ISizeType {
        TypeRef(PhantomData)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TypeRef<'compiler, 'ctx>(PhantomData<ContextRef<'compiler, 'ctx>>);

impl backend::TypeRef for TypeRef<'_, '_> {}

#[derive(Clone, Debug)]
pub struct BackendImpl;

impl backend::Backend for BackendImpl {
    type CompiledCode = CompiledCode;

    fn with_compiler<F: backend::CallWithCompiler>(
        &self,
        f: F,
    ) -> Result<Self::CompiledCode, F::Error> {
        let code = Code(());
        f.call_with_compiler(CompilerRef(&code))?;
        Ok(CompiledCode(code))
    }
}
