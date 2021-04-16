// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use core::fmt;

pub trait CompiledCode: fmt::Debug + 'static + Send + Sync {}

pub trait TypeRef: fmt::Debug + Copy {}

macro_rules! impl_context_types {
    (
        $(fn $type_fn:ident() -> Self::$type:ident;)*
    ) => {
        type Type: TypeRef $(+ From<Self::$type>)*;
        $(
            type $type: TypeRef;
            fn $type_fn(self) -> Self::$type;
        )*
    };
}

pub trait ContextRef: fmt::Debug + Copy {
    impl_context_types! {
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

pub trait CallWithContext {
    type Output;
    fn call_with_context<ContextT: ContextRef>(self, context: ContextT) -> Self::Output;
}

pub trait CompilerRef: fmt::Debug + Copy {
    fn with_context<F: CallWithContext>(self, f: F) -> F::Output;
}

pub trait CallWithCompiler {
    type Error;
    fn call_with_compiler<CompilerT: CompilerRef>(
        self,
        compiler: CompilerT,
    ) -> Result<(), Self::Error>;
}

pub trait Backend: Clone + Send + Sync + 'static + fmt::Debug {
    type CompiledCode: CompiledCode;
    fn with_compiler<F: CallWithCompiler>(&self, f: F) -> Result<Self::CompiledCode, F::Error>;
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

    mod test_create_context {
        use super::*;

        struct MyFn;

        impl CallWithBackend for MyFn {
            type Output = ();
            fn call<T: Backend>(self, backend: &T) -> Self::Output {
                backend.with_compiler(self).unwrap_err()
            }
        }

        impl CallWithCompiler for MyFn {
            type Error = ();

            fn call_with_compiler<CompilerT: CompilerRef>(
                self,
                compiler: CompilerT,
            ) -> Result<(), Self::Error> {
                compiler.with_context(self);
                Err(())
            }
        }

        impl CallWithContext for MyFn {
            type Output = ();

            fn call_with_context<ContextT: ContextRef>(self, context: ContextT) -> Self::Output {}
        }

        #[test]
        fn test_create_context() {
            BackendEnum::default().call_with(MyFn);
        }
    }
}
