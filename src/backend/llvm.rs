// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use crate::backend;
use inkwell::{
    context::Context,
    targets::TargetData,
    types::{AnyTypeEnum, FloatType, IntType},
};

#[cfg(not(any(
    feature = "backend-llvm-11",
    feature = "backend-llvm-10",
    feature = "backend-llvm-9",
    feature = "backend-llvm-8"
)))]
compile_error!("a cargo feature for specific LLVM version needs to be enabled, e.g. enable feature backend-llvm-11");

impl<'ctx> backend::Type<'ctx> for AnyTypeEnum<'ctx> {}

impl<'ctx> backend::Type<'ctx> for IntType<'ctx> {}

impl<'ctx> backend::Type<'ctx> for FloatType<'ctx> {}

#[derive(Debug)]
pub struct ContextImpl<'ctx> {
    context: &'ctx Context,
    target_data: TargetData,
}

macro_rules! impl_context_types {
    (
        impl<$ctx:lifetime> {
            $(#[type = $type:ident] fn $fn:ident() -> $ty:ty;)*
        }
    ) => {
        $(
            type $type = $ty;
            fn $fn(&$ctx self) -> Self::$type {
                self.context.$fn()
            }
        )*
    };
}

impl<'ctx> backend::Context<'ctx> for ContextImpl<'ctx> {
    type Type = AnyTypeEnum<'ctx>;

    type BoolType = IntType<'ctx>;

    fn bool_type(&'ctx self) -> Self::BoolType {
        self.context.custom_width_int_type(1)
    }

    impl_context_types! {
        impl<'ctx> {
            #[type = F32Type]
            fn f32_type() -> FloatType<'ctx>;
            #[type = F64Type]
            fn f64_type() -> FloatType<'ctx>;
            #[type = I8Type]
            fn i8_type() -> IntType<'ctx>;
            #[type = I16Type]
            fn i16_type() -> IntType<'ctx>;
            #[type = I32Type]
            fn i32_type() -> IntType<'ctx>;
            #[type = I64Type]
            fn i64_type() -> IntType<'ctx>;
            #[type = I128Type]
            fn i128_type() -> IntType<'ctx>;
        }
    }

    type ISizeType = IntType<'ctx>;

    fn isize_type(&'ctx self) -> Self::ISizeType {
        self.context.ptr_sized_int_type(&self.target_data, None)
    }

    type TypeTryFromError = ();
}

#[derive(Copy, Clone, Debug)]
pub struct BackendImpl;

impl<'ctx> backend::BackendCreateContext<'ctx> for BackendImpl {
    type Context = ContextImpl<'ctx>;
    type ContextInner = Context;

    fn create_context_inner() -> Self::ContextInner {
        Context::create()
    }

    fn create_context(inner: &'ctx Self::ContextInner) -> Self::Context {
        let context = inner;
        ContextImpl {
            context,
            target_data: todo!(),
        }
    }
}

impl backend::Backend for BackendImpl {}
