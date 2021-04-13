// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::backend;

#[derive(Debug, Clone)]
pub struct Type(());

impl<'ctx> backend::Type<'ctx> for Type {}

#[derive(Debug)]
pub struct Context(());

impl<'ctx> backend::Context<'ctx> for Context {
    type Type = Type;

    type BoolType = Type;

    fn bool_type(&'ctx self) -> Self::BoolType {
        Type(())
    }

    type F32Type = Type;

    fn f32_type(&'ctx self) -> Self::F32Type {
        Type(())
    }

    type F64Type = Type;

    fn f64_type(&'ctx self) -> Self::F64Type {
        Type(())
    }

    type I8Type = Type;

    fn i8_type(&'ctx self) -> Self::I8Type {
        Type(())
    }

    type I16Type = Type;

    fn i16_type(&'ctx self) -> Self::I16Type {
        Type(())
    }

    type I32Type = Type;

    fn i32_type(&'ctx self) -> Self::I32Type {
        Type(())
    }

    type I64Type = Type;

    fn i64_type(&'ctx self) -> Self::I64Type {
        Type(())
    }

    type I128Type = Type;

    fn i128_type(&'ctx self) -> Self::I128Type {
        Type(())
    }

    type ISizeType = Type;

    fn isize_type(&'ctx self) -> Self::ISizeType {
        Type(())
    }

    type TypeTryFromError = core::convert::Infallible;
}

#[derive(Clone, Debug)]
pub struct BackendImpl;

impl<'ctx> backend::BackendCreateContext<'ctx> for BackendImpl {
    type ContextInner = ();

    type Context = Context;

    fn create_context_inner() -> Self::ContextInner {
        ()
    }

    fn create_context(_inner: &'ctx Self::ContextInner) -> Self::Context {
        Context(())
    }
}

impl backend::Backend for BackendImpl {}
