// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::backend::{self, BackendError};
use alloc::{rc::Rc, vec, vec::Vec};
use core::{
    cell::{Cell, RefCell},
    convert::TryInto,
    fmt,
    marker::PhantomData,
    mem::{self, ManuallyDrop},
    ptr,
};
use hashbrown::HashMap;
use std::{
    ffi::{CStr, CString},
    sync::Mutex,
};
use typed_arena::Arena;

mod wrappers;

use wrappers::{llvm_sys, Own, Ref};

struct ExecutionEngine {
    /// `execution_engine` must be dropped before `contexts`
    execution_engine: ManuallyDrop<Own<wrappers::LlvmExecutionEngine>>,
    /// `LlvmContext`s owned by `execution_engine`, must not access them from outside `execution_engine`.
    contexts: Mutex<Vec<Own<wrappers::LlvmContext<'static>>>>,
}

impl Drop for ExecutionEngine {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.execution_engine) }
    }
}

impl fmt::Debug for ExecutionEngine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ExecutionEngine {{ ... }}")
    }
}

impl ExecutionEngine {
    /// must be called before calling `LLVMAddModule` or equivalent
    unsafe fn add_context<'compiler>(
        &'compiler self,
        context: Own<wrappers::LlvmContext<'compiler>>,
    ) {
        unsafe {
            self.contexts
                .lock()
                .unwrap()
                .push(context.transmute_lifetimes());
        }
    }
}

#[derive(Debug)]
pub struct CompiledCode {
    execution_engine: ExecutionEngine,
}

unsafe impl Send for CompiledCode {}

unsafe impl Sync for CompiledCode {}

impl backend::CompiledCode for CompiledCode {}

#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct TypeRef<'compiler, 'ctx>(Ref<'ctx, wrappers::LlvmType<'compiler>>);

impl<'compiler, 'ctx> TypeRef<'compiler, 'ctx> {
    fn as_slice_of_refs<'a>(slice: &'a [Self]) -> &'a [Ref<'ctx, wrappers::LlvmType<'compiler>>] {
        // Safety: `TypeRef` is a `#[repr(transparent)]` wrapper around `Ref<'ctx, wrappers::LlvmType<'compiler>>`
        unsafe { mem::transmute(slice) }
    }
}

impl<'compiler, 'ctx> backend::TypeRef for TypeRef<'compiler, 'ctx> {}

#[derive(Debug, Copy, Clone)]
pub struct FnPtrTypeRef<'compiler, 'ctx> {
    pointee: Ref<'ctx, wrappers::LlvmType<'compiler>>,
    abi: backend::FunctionABI,
}

impl<'compiler, 'ctx> backend::TypeRef for FnPtrTypeRef<'compiler, 'ctx> {}

impl<'compiler, 'ctx> From<FnPtrTypeRef<'compiler, 'ctx>> for TypeRef<'compiler, 'ctx> {
    fn from(v: FnPtrTypeRef<'compiler, 'ctx>) -> Self {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMPointerType(
                v.pointee.as_raw_ptr(),
                0,
            )))
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct ValueRef<'compiler, 'ctx>(Ref<'ctx, wrappers::LlvmValue<'compiler>>);

impl<'compiler, 'ctx> ValueRef<'compiler, 'ctx> {
    fn as_slice_of_refs<'a>(slice: &'a [Self]) -> &'a [Ref<'ctx, wrappers::LlvmValue<'compiler>>] {
        // Safety: `ValueRef` is a `#[repr(transparent)]` wrapper around `Ref<'ctx, wrappers::LlvmValue<'compiler>>`
        unsafe { mem::transmute(slice) }
    }
}

impl<'compiler, 'ctx> backend::ValueRef for ValueRef<'compiler, 'ctx> {}

#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct LabelRef<'compiler, 'ctx>(Ref<'ctx, wrappers::LlvmBasicBlock<'compiler>>);

impl<'compiler, 'ctx> LabelRef<'compiler, 'ctx> {
    fn as_slice_of_refs<'a>(
        slice: &'a [Self],
    ) -> &'a [Ref<'ctx, wrappers::LlvmBasicBlock<'compiler>>] {
        // Safety: `LabelRef` is a `#[repr(transparent)]` wrapper around `Ref<'ctx, wrappers::LlvmBasicBlock<'compiler>>`
        unsafe { mem::transmute(slice) }
    }
}

impl<'compiler, 'ctx> backend::LabelRef for LabelRef<'compiler, 'ctx> {}

#[derive(Clone, Default)]
struct BuilderCache<'compiler, 'ctx>(Rc<Cell<Option<Own<wrappers::LlvmBuilder<'compiler, 'ctx>>>>>);

impl fmt::Debug for BuilderCache<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BuilderCache {{ ... }}")
    }
}

#[derive(Debug)]
struct BuilderAndCache<'compiler, 'ctx> {
    builder: ManuallyDrop<Own<wrappers::LlvmBuilder<'compiler, 'ctx>>>,
    builder_cache: BuilderCache<'compiler, 'ctx>,
}

impl<'compiler, 'ctx> BuilderAndCache<'compiler, 'ctx> {
    fn new(
        context: Ref<'ctx, wrappers::LlvmContext<'compiler>>,
        builder_cache: BuilderCache<'compiler, 'ctx>,
    ) -> Self {
        let builder = builder_cache
            .0
            .take()
            .unwrap_or_else(|| wrappers::LlvmBuilder::new(context));
        Self {
            builder: ManuallyDrop::new(builder),
            builder_cache,
        }
    }
}

impl Drop for BuilderAndCache<'_, '_> {
    fn drop(&mut self) {
        let builder = unsafe { ptr::read(&*self.builder) };
        self.builder_cache.0.set(Some(builder));
    }
}

#[derive(Debug)]
pub struct BasicBlockBuilder<'compiler, 'ctx, 'modules> {
    module: ModuleRef<'compiler, 'ctx, 'modules>,
    fn_ptr: ValueRef<'compiler, 'ctx>,
    label: LabelRef<'compiler, 'ctx>,
    builder: BuilderAndCache<'compiler, 'ctx>,
}

impl<'compiler, 'ctx, 'modules> backend::BasicBlockBuilder
    for BasicBlockBuilder<'compiler, 'ctx, 'modules>
{
    type Context = ContextRef<'compiler, 'ctx, 'modules>;

    type Module = ModuleRef<'compiler, 'ctx, 'modules>;

    type FunctionBuilder = FunctionBuilder<'compiler, 'ctx, 'modules>;

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

    fn build_tail_call(
        self,
        fn_ptr: Self::FnPtr,
        fn_ptr_type: <Self::Module as backend::ModuleRef>::FnPtrType,
        arguments: &[Self::Value],
    ) {
        todo!()
    }

    fn build_ret(self, retval: Option<Self::Value>) {
        todo!()
    }
}

#[derive(Debug)]
pub struct FunctionBuilder<'compiler, 'ctx, 'modules> {
    module: ModuleRef<'compiler, 'ctx, 'modules>,
    fn_ptr: ValueRef<'compiler, 'ctx>,
    arguments: Vec<ValueRef<'compiler, 'ctx>>,
    builder_cache: BuilderCache<'compiler, 'ctx>,
}

impl<'compiler, 'ctx, 'modules> backend::FunctionBuilder
    for FunctionBuilder<'compiler, 'ctx, 'modules>
{
    type Context = ContextRef<'compiler, 'ctx, 'modules>;

    type Module = ModuleRef<'compiler, 'ctx, 'modules>;

    type Value = ValueRef<'compiler, 'ctx>;

    type FnPtr = ValueRef<'compiler, 'ctx>;

    type Label = LabelRef<'compiler, 'ctx>;

    type BasicBlockBuilder = BasicBlockBuilder<'compiler, 'ctx, 'modules>;

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
        let block_name = CString::new(block_name).unwrap();
        let builder = BuilderAndCache::new(self.module.context.context, self.builder_cache.clone());
        unsafe {
            let label = LabelRef(Ref::from_raw_ptr(
                llvm_sys::core::LLVMAppendBasicBlockInContext(
                    self.module.context.context.as_raw_ptr(),
                    self.fn_ptr.0.as_raw_ptr(),
                    block_name.as_ptr(),
                ),
            ));
            llvm_sys::core::LLVMPositionBuilderAtEnd(
                builder.builder.as_raw_ptr(),
                label.0.as_raw_ptr(),
            );
            BasicBlockBuilder {
                module: self.module,
                fn_ptr: self.fn_ptr,
                label,
                builder,
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ModuleRef<'compiler, 'ctx, 'modules> {
    module: Ref<'modules, wrappers::LlvmModule<'compiler, 'ctx>>,
    submitted: &'modules Cell<bool>,
    context: ContextRef<'compiler, 'ctx, 'modules>,
}

fn get_llvm_call_conv(abi: backend::FunctionABI) -> llvm_sys::LLVMCallConv {
    match abi {
        backend::FunctionABI::C => llvm_sys::LLVMCallConv::LLVMCCallConv,
        backend::FunctionABI::Fast => llvm_sys::LLVMCallConv::LLVMFastCallConv,
    }
}

impl<'compiler, 'ctx, 'modules> backend::ModuleRef for ModuleRef<'compiler, 'ctx, 'modules> {
    type Context = ContextRef<'compiler, 'ctx, 'modules>;

    fn context(self) -> Self::Context {
        self.context
    }

    unsafe fn submit_for_compilation(self) {
        self.submitted.set(true);
    }

    type Value = ValueRef<'compiler, 'ctx>;

    type FnPtr = ValueRef<'compiler, 'ctx>;

    type FunctionBuilder = FunctionBuilder<'compiler, 'ctx, 'modules>;

    type FnPtrType = FnPtrTypeRef<'compiler, 'ctx>;

    fn add_function_definition(
        self,
        name: &str,
        fn_ptr_type: Self::FnPtrType,
        entry_block_name: &str,
    ) -> backend::FunctionAndEntry<Self::FunctionBuilder> {
        let name = CString::new(name).unwrap();
        unsafe {
            let fn_ptr = ValueRef(Ref::from_raw_ptr(llvm_sys::core::LLVMAddFunction(
                self.module.as_raw_ptr(),
                name.as_ptr(),
                fn_ptr_type.pointee.as_raw_ptr(),
            )));
            llvm_sys::core::LLVMSetFunctionCallConv(
                fn_ptr.0.as_raw_ptr(),
                get_llvm_call_conv(fn_ptr_type.abi) as _,
            );
            let builder_cache = BuilderCache::default();
            let argument_count = llvm_sys::core::LLVMCountParams(fn_ptr.0.as_raw_ptr());
            let mut arguments = Vec::with_capacity(argument_count.try_into().unwrap());
            for i in 0..argument_count {
                arguments.push(ValueRef(Ref::from_raw_ptr(llvm_sys::core::LLVMGetParam(
                    fn_ptr.0.as_raw_ptr(),
                    i,
                ))));
            }
            let mut function = FunctionBuilder {
                module: self,
                fn_ptr,
                arguments,
                builder_cache,
            };
            let entry = backend::FunctionBuilder::add_block(&mut function, entry_block_name);
            backend::FunctionAndEntry { function, entry }
        }
    }

    fn add_function_declaration(self, name: &str, fn_ptr_type: Self::FnPtrType) -> Self::FnPtr {
        let name = CString::new(name).unwrap();
        unsafe {
            let fn_ptr = ValueRef(Ref::from_raw_ptr(llvm_sys::core::LLVMAddFunction(
                self.module.as_raw_ptr(),
                name.as_ptr(),
                fn_ptr_type.pointee.as_raw_ptr(),
            )));
            llvm_sys::core::LLVMSetFunctionCallConv(
                fn_ptr.0.as_raw_ptr(),
                get_llvm_call_conv(fn_ptr_type.abi) as _,
            );
            fn_ptr
        }
    }
}

#[derive(Debug)]
struct ModuleState<'compiler, 'ctx> {
    submitted: Cell<bool>,
    module: Own<wrappers::LlvmModule<'compiler, 'ctx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ContextRef<'compiler, 'ctx, 'modules> {
    context: Ref<'ctx, wrappers::LlvmContext<'compiler>>,
    context_with_modules: &'modules ContextWithModules<'compiler, 'ctx>,
    target_data: Ref<'ctx, wrappers::LlvmTargetData>,
    _phantom: PhantomData<&'modules mut ()>,
}

impl<'compiler: 'ctx, 'ctx, 'modules> backend::ContextRef
    for ContextRef<'compiler, 'ctx, 'modules>
{
    type Type = TypeRef<'compiler, 'ctx>;

    type ScalarType = TypeRef<'compiler, 'ctx>;

    type BoolType = TypeRef<'compiler, 'ctx>;

    fn bool_type(self) -> Self::BoolType {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMInt1TypeInContext(
                self.context.as_raw_ptr(),
            )))
        }
    }

    type F32Type = TypeRef<'compiler, 'ctx>;

    fn f32_type(self) -> Self::F32Type {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMFloatTypeInContext(
                self.context.as_raw_ptr(),
            )))
        }
    }

    type F64Type = TypeRef<'compiler, 'ctx>;

    fn f64_type(self) -> Self::F64Type {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMDoubleTypeInContext(
                self.context.as_raw_ptr(),
            )))
        }
    }

    type I8Type = TypeRef<'compiler, 'ctx>;

    fn i8_type(self) -> Self::I8Type {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMInt8TypeInContext(
                self.context.as_raw_ptr(),
            )))
        }
    }

    type I16Type = TypeRef<'compiler, 'ctx>;

    fn i16_type(self) -> Self::I16Type {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMInt16TypeInContext(
                self.context.as_raw_ptr(),
            )))
        }
    }

    type I32Type = TypeRef<'compiler, 'ctx>;

    fn i32_type(self) -> Self::I32Type {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMInt32TypeInContext(
                self.context.as_raw_ptr(),
            )))
        }
    }

    type I64Type = TypeRef<'compiler, 'ctx>;

    fn i64_type(self) -> Self::I64Type {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMInt64TypeInContext(
                self.context.as_raw_ptr(),
            )))
        }
    }

    type I128Type = TypeRef<'compiler, 'ctx>;

    fn i128_type(self) -> Self::I128Type {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMInt128TypeInContext(
                self.context.as_raw_ptr(),
            )))
        }
    }

    type ISizeType = TypeRef<'compiler, 'ctx>;

    fn isize_type(self) -> Self::ISizeType {
        unsafe {
            TypeRef(Ref::from_raw_ptr(
                llvm_sys::target::LLVMIntPtrTypeInContext(
                    self.context.as_raw_ptr(),
                    self.target_data.as_raw_ptr(),
                ),
            ))
        }
    }

    type PtrType = TypeRef<'compiler, 'ctx>;

    fn ptr_type(self, target: Self::Type) -> Self::PtrType {
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMPointerType(
                target.0.as_raw_ptr(),
                0,
            )))
        }
    }

    type StructType = TypeRef<'compiler, 'ctx>;

    fn struct_type(self, fields: &[Self::Type]) -> Self::StructType {
        let fields: &[llvm_sys::prelude::LLVMTypeRef] =
            Ref::as_slice_of_raw_ptrs(TypeRef::as_slice_of_refs(fields));
        let length = fields.len().try_into().expect("too many fields in struct");
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMStructTypeInContext(
                self.context.as_raw_ptr(),
                // uses non-const pointer, but doesn't modify passed-in slice
                fields.as_ptr() as *mut llvm_sys::prelude::LLVMTypeRef,
                length,
                false as _,
            )))
        }
    }

    type ArrayType = TypeRef<'compiler, 'ctx>;

    fn array_type(self, element: Self::Type, length: usize) -> Self::ArrayType {
        let length = length.try_into().expect("array length too big");
        unsafe {
            TypeRef(Ref::from_raw_ptr(llvm_sys::core::LLVMArrayType(
                element.0.as_raw_ptr(),
                length,
            )))
        }
    }

    type FnPtrType = FnPtrTypeRef<'compiler, 'ctx>;

    fn fn_ptr_type(
        self,
        arguments: &[Self::Type],
        return_type: Option<Self::Type>,
        abi: backend::FunctionABI,
    ) -> Self::FnPtrType {
        let arguments: &[llvm_sys::prelude::LLVMTypeRef] =
            Ref::as_slice_of_raw_ptrs(TypeRef::as_slice_of_refs(arguments));
        let length = arguments
            .len()
            .try_into()
            .expect("too many function arguments");
        unsafe {
            let return_type = match return_type {
                Some(v) => v.0.as_raw_ptr(),
                None => llvm_sys::core::LLVMVoidTypeInContext(self.context.as_raw_ptr()),
            };
            FnPtrTypeRef {
                pointee: Ref::from_raw_ptr(llvm_sys::core::LLVMFunctionType(
                    return_type,
                    // uses non-const pointer, but doesn't modify passed-in slice
                    arguments.as_ptr() as *mut llvm_sys::prelude::LLVMTypeRef,
                    length,
                    false as _,
                )),
                abi,
            }
        }
    }

    type Module = ModuleRef<'compiler, 'ctx, 'modules>;

    fn add_module(self, name: &str) -> Self::Module {
        let name = CString::new(name).unwrap();
        let module = self.context_with_modules.modules.alloc(ModuleState {
            module: wrappers::LlvmModule::new(self.context, name),
            submitted: Cell::new(false),
        });
        let submitted = &module.submitted;
        let module = module.module.as_ref();
        module.set_data_layout(&self.context_with_modules.compiler.target_data);
        module.set_target_triple(&self.context_with_modules.compiler.target_triple);
        ModuleRef {
            module,
            context: self,
            submitted,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct CompilerRef<'compiler>(&'compiler Compiler);

struct Context<'compiler> {
    context: Ref<'static, wrappers::LlvmContext<'compiler>>,
    compiler: &'compiler Compiler,
    target_data: Own<wrappers::LlvmTargetData>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
struct TailCallInstructionCacheKey<'compiler, 'ctx> {
    function_type: llvm_sys::prelude::LLVMTypeRef,
    _phantom: PhantomData<FnPtrTypeRef<'compiler, 'ctx>>,
}

#[derive(Copy, Clone, Debug)]
struct TailCallInstructionCacheValue<'compiler, 'ctx> {
    template_call_instruction: Ref<'ctx, wrappers::LlvmValue<'compiler>>,
}

/// cache for `musttail call` instructions
#[derive(Default)]
struct TailCallInstructionCache<'compiler, 'ctx> {
    cache: RefCell<
        HashMap<
            TailCallInstructionCacheKey<'compiler, 'ctx>,
            TailCallInstructionCacheValue<'compiler, 'ctx>,
        >,
    >,
}

struct ContextWithModules<'compiler, 'ctx> {
    context: Ref<'ctx, wrappers::LlvmContext<'compiler>>,
    compiler: &'compiler Compiler,
    target_data: Ref<'ctx, wrappers::LlvmTargetData>,
    modules: Arena<ModuleState<'compiler, 'ctx>>,
    tail_call_instruction_cache: TailCallInstructionCache<'compiler, 'ctx>,
}

impl fmt::Debug for ContextWithModules<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ContextWithModules {{ ... }}")
    }
}

impl<'compiler, 'ctx> ContextWithModules<'compiler, 'ctx> {
    fn as_context_ref<'modules>(&'modules self) -> ContextRef<'compiler, 'ctx, 'modules> {
        ContextRef {
            context: self.context,
            context_with_modules: self,
            target_data: self.target_data,
            _phantom: PhantomData,
        }
    }
}

impl<'compiler> Context<'compiler> {
    /// `context` must outlive `Self`
    unsafe fn new(
        compiler_ref: CompilerRef<'compiler>,
        context: Ref<'_, wrappers::LlvmContext<'compiler>>,
    ) -> Self {
        Self {
            context: unsafe { context.transmute_lifetimes() },
            compiler: compiler_ref.0,
            target_data: wrappers::LlvmTargetData::new(&compiler_ref.0.target_data),
        }
    }
    fn with_modules<'ctx>(&'ctx self) -> ContextWithModules<'compiler, 'ctx> {
        ContextWithModules {
            context: self.context,
            compiler: self.compiler,
            target_data: self.target_data.as_ref(),
            modules: Arena::new(),
            tail_call_instruction_cache: TailCallInstructionCache::default(),
        }
    }
}

impl<'compiler> backend::CompilerRef for CompilerRef<'compiler> {
    fn with_context<F: backend::CallWithContext>(self, f: F) -> Result<F::Output, F::Error> {
        let mut llvm_context = Some(wrappers::LlvmContext::new());
        let context = unsafe { Context::new(self, llvm_context.as_ref().unwrap().as_ref()) };
        let context_with_modules = context.with_modules();
        let retval = f.call_with_context(context_with_modules.as_context_ref())?;
        let modules = context_with_modules.modules.into_vec();
        unsafe {
            for module in modules {
                if module.submitted.get() {
                    if let Some(llvm_context) = llvm_context.take() {
                        self.0.execution_engine.add_context(llvm_context);
                    }
                    self.0
                        .execution_engine
                        .execution_engine
                        .as_ref()
                        .add_module(module.module);
                }
            }
        }
        Ok(retval)
    }
}

pub fn get_llvm_initialization_lock() -> &'static Mutex<()> {
    static LOCK: once_cell::sync::Lazy<Mutex<()>> = once_cell::sync::Lazy::new(|| Mutex::new(()));
    &LOCK
}

#[derive(Debug)]
struct Compiler {
    execution_engine: ExecutionEngine,
    target_data: Own<wrappers::LlvmString>,
    target_triple: Own<wrappers::LlvmString>,
    target_cpu: Own<wrappers::LlvmString>,
    target_feature_string: Own<wrappers::LlvmString>,
}

impl Compiler {
    unsafe fn new<E: BackendError>(
        optimization_level: backend::OptimizationLevel,
    ) -> Result<Self, E> {
        let optimization_level = match optimization_level {
            backend::OptimizationLevel::Debug => 0,
            backend::OptimizationLevel::Release => 2,
        };
        unsafe {
            let _lock = get_llvm_initialization_lock().lock().unwrap();
            assert!(
                llvm_sys::core::LLVMIsMultithreaded() != 0,
                "LLVM was not compiled multithreaded!"
            );
            if llvm_sys::target::LLVM_InitializeNativeTarget() == 1 {
                drop(_lock);
                return Err(E::from_message("failed to initialize LLVM native target"));
            }
            llvm_sys::execution_engine::LLVMLinkInMCJIT();
        }

        let mut contexts = Mutex::new(vec![wrappers::LlvmContext::new()]);
        let execution_engine;
        unsafe {
            let module = wrappers::LlvmModule::new(
                contexts.get_mut().unwrap()[0].as_ref(),
                CStr::from_bytes_with_nul(b"\0").unwrap(),
            );
            execution_engine = ManuallyDrop::new(
                wrappers::LlvmExecutionEngine::create_jit(module, optimization_level)
                    .map_err(|e| E::from_error(e))?,
            );
        }
        let execution_engine = ExecutionEngine {
            execution_engine,
            contexts,
        };
        let target_data: Own<wrappers::LlvmString>;
        let target_triple: Own<wrappers::LlvmString>;
        let target_cpu: Own<wrappers::LlvmString>;
        let target_feature_string: Own<wrappers::LlvmString>;
        unsafe {
            target_data = execution_engine
                .execution_engine
                .as_ref()
                .target_data()
                .to_string();
            let target_machine = execution_engine.execution_engine.as_ref().target_machine();
            target_triple = target_machine.triple();
            target_cpu = target_machine.cpu();
            target_feature_string = target_machine.feature_string();
        }
        Ok(Compiler {
            execution_engine,
            target_data,
            target_triple,
            target_cpu,
            target_feature_string,
        })
    }
    fn finish<E: BackendError>(self) -> Result<CompiledCode, E> {
        Ok(CompiledCode {
            execution_engine: self.execution_engine,
        })
    }
}

#[derive(Copy, Clone, Debug)]
pub struct BackendImpl;

impl backend::Backend for BackendImpl {
    type CompiledCode = CompiledCode;

    fn with_compiler<F: backend::CallWithCompiler>(
        &self,
        optimization_level: backend::OptimizationLevel,
        f: F,
    ) -> Result<Self::CompiledCode, F::Error> {
        let compiler = unsafe { Compiler::new(optimization_level) }?;
        f.call_with_compiler(CompilerRef(&compiler))?;
        compiler.finish()
    }
}
