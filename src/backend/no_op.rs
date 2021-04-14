// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::backend;

#[derive(Debug)]
pub struct CompiledCode(());

impl backend::CompiledCode for CompiledCode {}

#[derive(Debug, Clone)]
pub struct Type(());

impl<'ctx> backend::Type<'ctx> for Type {}

#[derive(Debug)]
pub struct Context(());

impl<'ctx> backend::ContextRef<'ctx> for &'ctx Context {
    type CompiledCode = CompiledCode;

    type Type = Type;

    type BoolType = Type;

    fn bool_type(self) -> Self::BoolType {
        Type(())
    }

    type F32Type = Type;

    fn f32_type(self) -> Self::F32Type {
        Type(())
    }

    type F64Type = Type;

    fn f64_type(self) -> Self::F64Type {
        Type(())
    }

    type I8Type = Type;

    fn i8_type(self) -> Self::I8Type {
        Type(())
    }

    type I16Type = Type;

    fn i16_type(self) -> Self::I16Type {
        Type(())
    }

    type I32Type = Type;

    fn i32_type(self) -> Self::I32Type {
        Type(())
    }

    type I64Type = Type;

    fn i64_type(self) -> Self::I64Type {
        Type(())
    }

    type I128Type = Type;

    fn i128_type(self) -> Self::I128Type {
        Type(())
    }

    type ISizeType = Type;

    fn isize_type(self) -> Self::ISizeType {
        Type(())
    }
}

#[derive(Clone, Debug)]
pub struct BackendImpl;

impl backend::Backend for BackendImpl {
    type CompiledCode = CompiledCode;

    fn with_context<F: backend::CallWithContext<Self>>(&self, f: F) -> F::Output {
        f.call(self, &Context(()))
    }
}
