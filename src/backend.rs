// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use core::{convert::TryFrom, fmt};

pub trait CompiledCode: fmt::Debug + 'static + Send + Sync {}

pub trait Type<'ctx>: fmt::Debug + Clone {}

macro_rules! impl_context_types {
    (
        impl<$ctx:lifetime> {
            $(fn $type_fn:ident() -> Self::$type:ident;)*
        }
    ) => {
        type Type: Type<$ctx> $(+ From<Self::$type>)*;
        $(
            type $type: Type<$ctx>;
            fn $type_fn(self) -> Self::$type;
        )*
    };
}

pub trait ContextRef<'ctx>: fmt::Debug + Copy + 'ctx {
    type CompiledCode: CompiledCode;
    impl_context_types! {
        impl<'ctx> {
            fn bool_type() -> Self::BoolType;
            fn f32_type() -> Self::F32Type;
            fn f64_type() -> Self::F64Type;
            fn i8_type() -> Self::I8Type;
            fn i16_type() -> Self::I16Type;
            fn i32_type() -> Self::I32Type;
            fn i64_type() -> Self::I64Type;
            fn i128_type() -> Self::I128Type;
            fn isize_type() -> Self::ISizeType;
        }
    }
}

pub trait CallWithContext<B>
where
    B: Backend,
{
    type Output;
    fn call<'ctx, C: ContextRef<'ctx, CompiledCode = B::CompiledCode>>(
        self,
        backend: &B,
        context: C,
    ) -> Self::Output;
}

pub trait Backend: Clone + Send + Sync + 'static + fmt::Debug {
    type CompiledCode: CompiledCode;
    fn with_context<F: CallWithContext<Self>>(&self, f: F) -> F::Output;
}

pub trait CallWithBackend {
    type Output;
    fn call<T: Backend>(self, backend: &T) -> Self::Output;
}

#[cfg(feature = "backend-llvm")]
pub mod llvm;
#[cfg(feature = "backend-no-op")]
pub mod no_op;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum BackendEnum {
    #[cfg(feature = "backend-llvm")]
    Llvm,
    #[cfg(feature = "backend-no-op")]
    NoOp,
}

impl BackendEnum {
    pub fn call_with<F: CallWithBackend>(self, f: F) -> F::Output {
        match self {
            #[cfg(feature = "backend-llvm")]
            Self::Llvm => f.call(&llvm::BackendImpl),
            #[cfg(feature = "backend-no-op")]
            Self::NoOp => f.call(&no_op::BackendImpl),
        }
    }
}

impl Default for BackendEnum {
    fn default() -> Self {
        cfg_if::cfg_if! {
            if #[cfg(feature = "backend-llvm")] {
                Self::Llvm
            } else if #[cfg(feature = "backend-no-op")] {
                Self::NoOp
            } else {
                compile_error!("no selected backend, you need to enable one of the backend-* features")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_context() {
        struct MyFn;
        impl CallWithBackend for MyFn {
            type Output = ();
            fn call<T: Backend>(self, backend: &T) -> Self::Output {
                backend.with_context(self)
            }
        }
        impl<B: Backend> CallWithContext<B> for MyFn {
            type Output = ();
            fn call<'ctx, C: ContextRef<'ctx>>(self, backend: &B, context: C) -> Self::Output {}
        }
        BackendEnum::default().call_with(MyFn);
    }
}
