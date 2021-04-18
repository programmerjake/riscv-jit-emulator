// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use alloc::{boxed::Box, string::ToString};
use core::fmt;

pub trait CompiledCode: fmt::Debug + 'static + Send + Sync {}

pub trait TypeRef: fmt::Debug + Copy {}

pub trait BackendError: fmt::Display + fmt::Debug + 'static + Send + Sync + Sized {
    fn from_message<T: ToString>(message: T) -> Self;
    #[cfg(any(test, feature = "std"))]
    fn from_error<T: Into<Box<dyn std::error::Error + Send + Sync>>>(error: T) -> Self {
        Self::from_message(error.into())
    }
}

#[cfg(any(test, feature = "std"))]
impl BackendError for std::io::Error {
    fn from_message<T: ToString>(message: T) -> Self {
        Self::new(std::io::ErrorKind::Other, message.to_string())
    }
    fn from_error<T: Into<Box<dyn std::error::Error + Send + Sync>>>(error: T) -> Self {
        Self::new(std::io::ErrorKind::Other, error.into())
    }
}

pub trait Module: fmt::Debug {
    type ContextRef: ContextRef;
    fn context(&self) -> Self::ContextRef;
    fn submit_for_compilation(self);
}

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
    type Module: Module<ContextRef = Self>;
    fn create_module(self, name: &str) -> Self::Module;
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
    type Error: BackendError;
    fn call_with_context<ContextT: ContextRef>(
        self,
        context: ContextT,
    ) -> Result<Self::Output, Self::Error>;
}

pub trait CompilerRef: fmt::Debug + Copy {
    fn with_context<F: CallWithContext>(self, f: F) -> Result<F::Output, F::Error>;
}

pub trait CallWithCompiler {
    type Error: BackendError;
    fn call_with_compiler<CompilerT: CompilerRef>(
        self,
        compiler: CompilerT,
    ) -> Result<(), Self::Error>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OptimizationLevel {
    Debug,
    Release,
}

pub trait Backend: Clone + Send + Sync + 'static + fmt::Debug {
    type CompiledCode: CompiledCode;
    fn with_compiler<F: CallWithCompiler>(
        &self,
        optimization_level: OptimizationLevel,
        f: F,
    ) -> Result<Self::CompiledCode, F::Error>;
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

    #[derive(Debug)]
    enum MyError {
        Error(std::io::Error),
        DontWantToCompile,
    }

    impl fmt::Display for MyError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                MyError::Error(e) => e.fmt(f),
                MyError::DontWantToCompile => write!(f, "don't want to compile"),
            }
        }
    }

    impl std::error::Error for MyError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            match self {
                MyError::Error(e) => e.source(),
                MyError::DontWantToCompile => None,
            }
        }
    }

    impl BackendError for MyError {
        fn from_message<T: ToString>(message: T) -> Self {
            std::io::Error::from_message(message).into()
        }

        fn from_error<T: Into<Box<dyn std::error::Error + Send + Sync>>>(error: T) -> Self {
            std::io::Error::from_error(error).into()
        }
    }

    impl From<std::io::Error> for MyError {
        fn from(v: std::io::Error) -> Self {
            Self::Error(v)
        }
    }

    mod test_create_context {
        use super::*;

        struct MyFn;

        impl CallWithBackend for MyFn {
            type Output = ();
            fn call<T: Backend>(self, backend: &T) -> Self::Output {
                match backend.with_compiler(OptimizationLevel::Debug, self) {
                    Err(MyError::DontWantToCompile) => {}
                    result => panic!("unexpected result: {:?}", result),
                }
            }
        }

        impl CallWithCompiler for MyFn {
            type Error = MyError;

            fn call_with_compiler<CompilerT: CompilerRef>(
                self,
                compiler: CompilerT,
            ) -> Result<(), Self::Error> {
                compiler.with_context(self)?;
                Err(MyError::DontWantToCompile)
            }
        }

        impl CallWithContext for MyFn {
            type Output = ();
            type Error = MyError;

            fn call_with_context<ContextT: ContextRef>(
                self,
                context: ContextT,
            ) -> Result<Self::Output, Self::Error> {
                Ok(())
            }
        }

        #[test]
        fn test_create_context() {
            BackendEnum::default().call_with(MyFn);
        }
    }
}
