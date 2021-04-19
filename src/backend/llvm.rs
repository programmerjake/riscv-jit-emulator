// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::backend::{self, BackendError};
use alloc::{vec, vec::Vec};
use core::{
    cell::RefCell,
    fmt,
    marker::PhantomData,
    mem::{self, ManuallyDrop},
};
use std::{
    ffi::{CStr, CString},
    sync::Mutex,
};

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
pub struct TypeRef<'compiler, 'ctx>(Ref<'ctx, wrappers::LlvmType<'compiler>>);

impl<'compiler, 'ctx> backend::TypeRef for TypeRef<'compiler, 'ctx> {}

#[derive(Debug)]
pub struct Module<'compiler, 'ctx> {
    module: Own<wrappers::LlvmModule<'compiler, 'ctx>>,
    context: ContextRef<'compiler, 'ctx>,
}

impl<'compiler, 'ctx> backend::Module for Module<'compiler, 'ctx> {
    type ContextRef = ContextRef<'compiler, 'ctx>;

    fn context(&self) -> Self::ContextRef {
        self.context
    }

    fn submit_for_compilation(self) {
        unsafe {
            self.context
                .submitted_modules
                .borrow_mut()
                .push(self.module.transmute_lifetimes())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ContextRef<'compiler, 'ctx> {
    context: Ref<'ctx, wrappers::LlvmContext<'compiler>>,
    compiler: &'compiler Compiler,
    target_data: Ref<'ctx, wrappers::LlvmTargetData>,
    submitted_modules: &'ctx RefCell<Vec<Own<wrappers::LlvmModule<'static, 'static>>>>,
    _phantom: PhantomData<&'ctx mut ()>,
}

impl<'compiler: 'ctx, 'ctx> backend::ContextRef for ContextRef<'compiler, 'ctx> {
    type Type = TypeRef<'compiler, 'ctx>;

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

    type Module = Module<'compiler, 'ctx>;

    fn create_module(self, name: &str) -> Self::Module {
        let name = CString::new(name).unwrap();
        let module = wrappers::LlvmModule::new(self.context, name);
        module.as_ref().set_data_layout(&self.compiler.target_data);
        module
            .as_ref()
            .set_target_triple(&self.compiler.target_triple);
        Module {
            module,
            context: self,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct CompilerRef<'compiler>(&'compiler Compiler);

struct ContextWithoutDrop<'compiler> {
    context: Own<wrappers::LlvmContext<'compiler>>,
    compiler: &'compiler Compiler,
    target_data: Own<wrappers::LlvmTargetData>,
    // must be dropped before `context`
    submitted_modules: RefCell<Vec<Own<wrappers::LlvmModule<'static, 'static>>>>,
}

#[repr(transparent)]
struct Context<'compiler>(ContextWithoutDrop<'compiler>);

impl<'compiler> Context<'compiler> {
    fn new(compiler_ref: CompilerRef<'compiler>) -> Self {
        Self(ContextWithoutDrop {
            context: wrappers::LlvmContext::new(),
            compiler: compiler_ref.0,
            target_data: wrappers::LlvmTargetData::new(&compiler_ref.0.target_data),
            submitted_modules: Default::default(),
        })
    }
    fn as_ref<'ctx>(&'ctx self) -> ContextRef<'compiler, 'ctx> {
        ContextRef {
            context: self.0.context.as_ref(),
            compiler: self.0.compiler,
            target_data: self.0.target_data.as_ref(),
            submitted_modules: &self.0.submitted_modules,
            _phantom: PhantomData,
        }
    }
    unsafe fn into_context_without_drop(self) -> ContextWithoutDrop<'compiler> {
        unsafe { mem::transmute(self) }
    }
}

impl Drop for Context<'_> {
    fn drop(&mut self) {
        self.0.submitted_modules.take();
    }
}

impl<'compiler> backend::CompilerRef for CompilerRef<'compiler> {
    fn with_context<F: backend::CallWithContext>(self, f: F) -> Result<F::Output, F::Error> {
        let context = Context::new(self);
        let retval = f.call_with_context(context.as_ref())?;
        if context.0.submitted_modules.borrow().is_empty() {
            return Ok(retval);
        }
        unsafe {
            let ContextWithoutDrop {
                context,
                compiler,
                target_data: _,
                submitted_modules,
            } = context.into_context_without_drop();
            compiler.execution_engine.add_context(context);
            for submitted_module in submitted_modules.into_inner() {
                compiler
                    .execution_engine
                    .execution_engine
                    .as_ref()
                    .add_module(submitted_module.transmute_lifetimes());
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
