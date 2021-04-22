// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::backend;
use alloc::{vec, vec::Vec};
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
    fn with_context<F: backend::CallWithContext>(self, f: F) -> Result<F::Output, F::Error> {
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

    type ScalarType = TypeRef<'compiler, 'ctx>;

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

    type PtrType = TypeRef<'compiler, 'ctx>;

    fn ptr_type(self, _target: Self::Type) -> Self::PtrType {
        TypeRef(PhantomData)
    }

    type StructType = TypeRef<'compiler, 'ctx>;

    fn struct_type(self, _fields: &[Self::Type]) -> Self::StructType {
        TypeRef(PhantomData)
    }

    type ArrayType = TypeRef<'compiler, 'ctx>;

    fn array_type(self, _element: Self::Type, _length: usize) -> Self::ArrayType {
        TypeRef(PhantomData)
    }

    type FnPtrType = FnPtrTypeRef<'compiler, 'ctx>;

    fn fn_ptr_type(
        self,
        arguments: &[Self::Type],
        _return_type: Option<Self::Type>,
        _abi: backend::FunctionABI,
    ) -> Self::FnPtrType {
        FnPtrTypeRef {
            ty: TypeRef(PhantomData),
            argument_count: arguments.len(),
        }
    }

    type Module = ModuleRef<'compiler, 'ctx>;

    fn add_module(self, _name: &str) -> Self::Module {
        ModuleRef { context: self }
    }
}

#[derive(Debug)]
pub struct BasicBlockBuilder<'compiler, 'ctx> {
    fn_ptr: ValueRef<'compiler, 'ctx>,
    label: LabelRef<'compiler, 'ctx>,
    module: ModuleRef<'compiler, 'ctx>,
}

impl<'compiler, 'ctx> backend::BasicBlockBuilder for BasicBlockBuilder<'compiler, 'ctx> {
    type Context = ContextRef<'compiler, 'ctx>;

    type Module = ModuleRef<'compiler, 'ctx>;

    type Value = ValueRef<'compiler, 'ctx>;

    type FnPtr = ValueRef<'compiler, 'ctx>;

    type Label = LabelRef<'compiler, 'ctx>;

    fn module(&self) -> Self::Module {
        self.module
    }

    fn fn_ptr(&self) -> Self::FnPtr {
        self.fn_ptr
    }

    fn label(&self) -> Self::Label {
        self.label
    }

    type FunctionBuilder = FunctionBuilder<'compiler, 'ctx>;

    fn build_tail_call(
        self,
        fn_ptr: Self::FnPtr,
        fn_ptr_type: <Self::Module as backend::ModuleRef>::FnPtrType,
        arguments: &[Self::Value],
    ) {
    }

    fn build_ret(self, retval: Option<Self::Value>) {}
}

#[derive(Debug)]
pub struct FunctionBuilder<'compiler, 'ctx> {
    fn_ptr: ValueRef<'compiler, 'ctx>,
    arguments: Vec<ValueRef<'compiler, 'ctx>>,
    module: ModuleRef<'compiler, 'ctx>,
}

impl<'compiler, 'ctx> backend::FunctionBuilder for FunctionBuilder<'compiler, 'ctx> {
    type Context = ContextRef<'compiler, 'ctx>;

    type Module = ModuleRef<'compiler, 'ctx>;

    type Value = ValueRef<'compiler, 'ctx>;

    type FnPtr = ValueRef<'compiler, 'ctx>;

    type Label = LabelRef<'compiler, 'ctx>;

    type BasicBlockBuilder = BasicBlockBuilder<'compiler, 'ctx>;

    fn module(&self) -> Self::Module {
        self.module
    }

    fn fn_ptr(&self) -> Self::FnPtr {
        self.fn_ptr
    }

    fn arguments(&self) -> &[Self::Value] {
        &self.arguments
    }

    fn add_block(&mut self, block_name: &str) -> Self::BasicBlockBuilder {
        BasicBlockBuilder {
            fn_ptr: self.fn_ptr,
            label: LabelRef(PhantomData),
            module: self.module,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ModuleRef<'compiler, 'ctx> {
    context: ContextRef<'compiler, 'ctx>,
}

impl<'compiler, 'ctx> backend::ModuleRef for ModuleRef<'compiler, 'ctx> {
    type Context = ContextRef<'compiler, 'ctx>;

    fn context(self) -> Self::Context {
        self.context
    }

    unsafe fn submit_for_compilation(self) {}

    type Value = ValueRef<'compiler, 'ctx>;

    type FnPtr = ValueRef<'compiler, 'ctx>;

    type FunctionBuilder = FunctionBuilder<'compiler, 'ctx>;

    type FnPtrType = FnPtrTypeRef<'compiler, 'ctx>;

    fn add_function_definition(
        self,
        name: &str,
        fn_ptr_type: Self::FnPtrType,
        entry_block_name: &str,
    ) -> backend::FunctionAndEntry<Self::FunctionBuilder> {
        let fn_ptr = ValueRef(PhantomData);
        let function = FunctionBuilder {
            fn_ptr,
            module: self,
            arguments: vec![ValueRef(PhantomData); fn_ptr_type.argument_count],
        };
        let entry = BasicBlockBuilder {
            fn_ptr,
            label: LabelRef(PhantomData),
            module: self,
        };
        backend::FunctionAndEntry { function, entry }
    }

    fn add_function_declaration(self, name: &str, fn_ptr_type: Self::FnPtrType) -> Self::FnPtr {
        ValueRef(PhantomData)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TypeRef<'compiler, 'ctx>(PhantomData<ContextRef<'compiler, 'ctx>>);

impl backend::TypeRef for TypeRef<'_, '_> {}

#[derive(Debug, Copy, Clone)]
pub struct FnPtrTypeRef<'compiler, 'ctx> {
    ty: TypeRef<'compiler, 'ctx>,
    argument_count: usize,
}

impl backend::TypeRef for FnPtrTypeRef<'_, '_> {}

impl<'compiler, 'ctx> From<FnPtrTypeRef<'compiler, 'ctx>> for TypeRef<'compiler, 'ctx> {
    fn from(v: FnPtrTypeRef<'compiler, 'ctx>) -> Self {
        v.ty
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ValueRef<'compiler, 'ctx>(PhantomData<ContextRef<'compiler, 'ctx>>);

impl backend::ValueRef for ValueRef<'_, '_> {}

#[derive(Debug, Copy, Clone)]
pub struct LabelRef<'compiler, 'ctx>(PhantomData<ContextRef<'compiler, 'ctx>>);

impl backend::LabelRef for LabelRef<'_, '_> {}

#[derive(Clone, Debug)]
pub struct BackendImpl;

impl backend::Backend for BackendImpl {
    type CompiledCode = CompiledCode;

    fn with_compiler<F: backend::CallWithCompiler>(
        &self,
        _optimization_level: backend::OptimizationLevel,
        f: F,
    ) -> Result<Self::CompiledCode, F::Error> {
        let code = Code(());
        f.call_with_compiler(CompilerRef(&code))?;
        Ok(CompiledCode(code))
    }
}
