// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use alloc::{boxed::Box, string::ToString};
use core::fmt;

pub mod types;

pub trait CompiledCode: fmt::Debug + 'static + Send + Sync {}

pub trait TypeRef: fmt::Debug + Copy {}

pub trait ValueRef: fmt::Debug + Copy {}

pub trait LabelRef: fmt::Debug + Copy {}

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

pub trait BasicBlockBuilder: fmt::Debug {
    type Context: ContextRef;
    type Module: ModuleRef<
        Context = Self::Context,
        Value = Self::Value,
        FnPtr = Self::FnPtr,
        FunctionBuilder = Self::FunctionBuilder,
    >;
    type FunctionBuilder: FunctionBuilder<
        Context = Self::Context,
        Module = Self::Module,
        Value = Self::Value,
        FnPtr = Self::FnPtr,
        Label = Self::Label,
        BasicBlockBuilder = Self,
    >;
    type Value: ValueRef + From<Self::FnPtr>;
    type FnPtr: ValueRef;
    type Label: LabelRef;
    #[must_use]
    fn context(&self) -> Self::Context {
        self.module().context()
    }
    #[must_use]
    fn module(&self) -> Self::Module;
    #[must_use]
    fn fn_ptr(&self) -> Self::FnPtr;
    #[must_use]
    fn label(&self) -> Self::Label;
    fn build_ret(self, retval: Option<Self::Value>);
    fn build_tail_call(
        self,
        fn_ptr: Self::FnPtr,
        fn_ptr_type: <Self::Module as ModuleRef>::FnPtrType,
        arguments: &[Self::Value],
    );
}

pub trait FunctionBuilder: fmt::Debug {
    type Context: ContextRef;
    type Module: ModuleRef<
        Context = Self::Context,
        Value = Self::Value,
        FnPtr = Self::FnPtr,
        FunctionBuilder = Self,
    >;
    type Value: ValueRef + From<Self::FnPtr>;
    type FnPtr: ValueRef;
    type Label: LabelRef;
    type BasicBlockBuilder: BasicBlockBuilder<
        Context = Self::Context,
        Module = Self::Module,
        FunctionBuilder = Self,
        Value = Self::Value,
        FnPtr = Self::FnPtr,
        Label = Self::Label,
    >;
    #[must_use]
    fn context(&self) -> Self::Context {
        self.module().context()
    }
    #[must_use]
    fn module(&self) -> Self::Module;
    #[must_use]
    fn fn_ptr(&self) -> Self::FnPtr;
    #[must_use]
    fn arguments(&self) -> &[Self::Value];
    #[must_use]
    fn add_block(&mut self, block_name: &str) -> Self::BasicBlockBuilder;
}

#[must_use]
#[derive(Debug)]
pub struct FunctionAndEntry<F: FunctionBuilder> {
    pub function: F,
    pub entry: F::BasicBlockBuilder,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum FunctionABI {
    C,
    Fast,
}

pub trait ModuleRef: fmt::Debug + Copy {
    type Context: ContextRef<Module = Self, FnPtrType = Self::FnPtrType>;
    type Value: ValueRef + From<Self::FnPtr>;
    type FnPtr: ValueRef;
    type FunctionBuilder: FunctionBuilder<
        Context = Self::Context,
        Value = Self::Value,
        FnPtr = Self::FnPtr,
        Module = Self,
    >;
    type FnPtrType: TypeRef;
    #[must_use]
    fn context(self) -> Self::Context;
    fn add_function_definition(
        self,
        name: &str,
        fn_ptr_type: Self::FnPtrType,
        entry_block_name: &str,
    ) -> FunctionAndEntry<Self::FunctionBuilder>;
    fn add_function_declaration(self, name: &str, fn_ptr_type: Self::FnPtrType) -> Self::FnPtr;
    /// Safety: this module must meet all the code validity requirements
    unsafe fn submit_for_compilation(self);
}

macro_rules! impl_context_types {
    (
        $(#[scalar] fn $scalar_type_fn:ident($($scalar_args:tt)*) -> Self::$scalar_type:ident;)*
        $(fn $type_fn:ident($($args:tt)*) -> Self::$type:ident;)*
    ) => {
        type Type: TypeRef $(+ From<Self::$scalar_type>)* $(+ From<Self::$type>)* + From<Self::ScalarType>;
        type ScalarType: TypeRef $(+ From<Self::$scalar_type>)*;
        $(
            type $scalar_type: TypeRef;
            fn $scalar_type_fn($($scalar_args)*) -> Self::$scalar_type;
        )*
        $(
            type $type: TypeRef;
            fn $type_fn($($args)*) -> Self::$type;
        )*
    };
}

pub trait ContextRef: fmt::Debug + Copy {
    type Module: ModuleRef<Context = Self, FnPtrType = Self::FnPtrType>;
    #[must_use]
    fn add_module(self, name: &str) -> Self::Module;
    impl_context_types! {
        #[scalar]
        fn bool_type(self) -> Self::BoolType;
        #[scalar]
        fn f32_type(self) -> Self::F32Type;
        #[scalar]
        fn f64_type(self) -> Self::F64Type;
        #[scalar]
        fn i8_type(self) -> Self::I8Type;
        #[scalar]
        fn i16_type(self) -> Self::I16Type;
        #[scalar]
        fn i32_type(self) -> Self::I32Type;
        #[scalar]
        fn i64_type(self) -> Self::I64Type;
        #[scalar]
        fn i128_type(self) -> Self::I128Type;
        #[scalar]
        fn isize_type(self) -> Self::ISizeType;
        #[scalar]
        fn ptr_type(self, target: Self::Type) -> Self::PtrType;
        fn struct_type(self, fields: &[Self::Type]) -> Self::StructType;
        fn array_type(self, element: Self::Type, length: usize) -> Self::ArrayType;
        fn fn_ptr_type(self, arguments: &[Self::Type], return_type: Option<Self::Type>, abi: FunctionABI) -> Self::FnPtrType;
    }
    #[must_use]
    fn type_for<T: types::TypeFor>(self) -> Self::Type {
        T::type_for(self)
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
                _context: ContextT,
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
