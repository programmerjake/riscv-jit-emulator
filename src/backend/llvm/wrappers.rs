// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use core::{
    fmt,
    marker::PhantomData,
    mem,
    ops::Deref,
    ptr::{self, NonNull},
    slice,
};
use std::{
    ffi::CStr,
    os::raw::{c_char, c_uint},
};

#[cfg(feature = "backend-llvm-10")]
pub(super) use llvm_sys_10 as llvm_sys;
#[cfg(feature = "backend-llvm-11")]
pub(super) use llvm_sys_11 as llvm_sys;

#[cfg(not(any(feature = "backend-llvm-11", feature = "backend-llvm-10")))]
compile_error!("a cargo feature for specific LLVM version needs to be enabled, e.g. enable feature backend-llvm-11");

pub(crate) unsafe trait Wrap: Sized {
    type Pointee: ?Sized;
    type PhantomData;
}

pub(crate) unsafe trait WrapTransmuteLifetimes<Other>: Wrap
where
    Other: Wrap<Pointee = Self::Pointee>,
{
}

pub(crate) unsafe trait WrapSync: Wrap {}

pub(crate) unsafe trait WrapSend: Wrap {}

pub(crate) trait WrapDeref: Wrap {
    type Target: ?Sized;
    fn deref<'a>(v: Ref<'a, Self>) -> &'a Self::Target;
}

pub(crate) trait WrapOwned: Wrap {
    unsafe fn do_drop(v: NonNull<Self::Pointee>);
}

pub(crate) trait WrapClone: WrapOwned {
    fn do_clone(v: Ref<'_, Self>) -> Own<Self>;
}

pub(crate) trait WrapEq: Wrap {
    fn eq(a: Ref<'_, Self>, b: Ref<'_, Self>) -> bool;
}

#[repr(transparent)]
pub(crate) struct Own<T: WrapOwned>(NonNull<T::Pointee>, PhantomData<T::PhantomData>);

impl<T: WrapOwned> Own<T> {
    pub(crate) unsafe fn from_raw_non_null(value: NonNull<T::Pointee>) -> Self {
        Self(value, PhantomData)
    }
    pub(crate) fn into_raw_non_null(self) -> NonNull<T::Pointee> {
        let retval = self.as_raw_non_null();
        mem::forget(self);
        retval
    }
    pub(crate) fn as_raw_non_null(&self) -> NonNull<T::Pointee> {
        self.0
    }
    pub(crate) unsafe fn from_raw_ptr(value: *mut T::Pointee) -> Self {
        unsafe { Self::from_raw_ptr_opt(value).unwrap() }
    }
    pub(crate) unsafe fn from_raw_ptr_opt(value: *mut T::Pointee) -> Option<Self> {
        unsafe { NonNull::new(value).map(|v| Self::from_raw_non_null(v)) }
    }
    pub(crate) fn into_raw_ptr(self) -> *mut T::Pointee {
        self.into_raw_non_null().as_ptr()
    }
    pub(crate) fn as_raw_ptr(&self) -> *mut T::Pointee {
        self.as_raw_non_null().as_ptr()
    }
    pub(crate) fn as_ref<'a>(&'a self) -> Ref<'a, T> {
        unsafe { mem::transmute_copy::<Own<T>, Ref<'a, T>>(self) }
    }
    pub(crate) unsafe fn transmute_lifetimes<Other>(self) -> Own<Other>
    where
        Other: Wrap<Pointee = T::Pointee> + WrapOwned + WrapTransmuteLifetimes<T>,
    {
        unsafe { Own::from_raw_non_null(self.into_raw_non_null()) }
    }
}

impl<T: WrapOwned> Drop for Own<T> {
    fn drop(&mut self) {
        unsafe { T::do_drop(self.as_raw_non_null()) }
    }
}

impl<T: WrapOwned + WrapDeref> Deref for Own<T> {
    type Target = T::Target;

    fn deref(&self) -> &Self::Target {
        T::deref(self.as_ref())
    }
}

impl<T: WrapOwned> fmt::Debug for Own<T>
where
    for<'a> Ref<'a, T>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<T: WrapOwned> fmt::Display for Own<T>
where
    for<'a> Ref<'a, T>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

unsafe impl<T: WrapOwned + WrapSend> Send for Own<T> {}

unsafe impl<T: WrapOwned + WrapSync> Sync for Own<T> {}

impl<T: WrapClone> Clone for Own<T> {
    fn clone(&self) -> Self {
        self.as_ref().to_owned()
    }
}

impl<T: WrapEq + WrapOwned> Eq for Own<T> {}

impl<T: WrapEq + WrapOwned> PartialEq<Ref<'_, T>> for Own<T> {
    fn eq(&self, other: &Ref<'_, T>) -> bool {
        WrapEq::eq(self.as_ref(), *other)
    }
}

impl<T: WrapEq + WrapOwned> PartialEq<Own<T>> for Ref<'_, T> {
    fn eq(&self, other: &Own<T>) -> bool {
        WrapEq::eq(*self, other.as_ref())
    }
}

impl<T: WrapEq + WrapOwned> PartialEq for Own<T> {
    fn eq(&self, other: &Own<T>) -> bool {
        WrapEq::eq(self.as_ref(), other.as_ref())
    }
}

#[repr(transparent)]
pub(crate) struct Ref<'a, T: Wrap>(NonNull<T::Pointee>, PhantomData<(&'a (), T::PhantomData)>);

impl<'a, T: Wrap> Ref<'a, T> {
    pub(crate) unsafe fn from_raw_non_null(value: NonNull<T::Pointee>) -> Self {
        Self(value, PhantomData)
    }
    pub(crate) fn as_raw_non_null(&self) -> NonNull<T::Pointee> {
        self.0
    }
    pub(crate) unsafe fn from_raw_ptr(value: *mut T::Pointee) -> Self {
        unsafe { Self::from_raw_ptr_opt(value).unwrap() }
    }
    pub(crate) unsafe fn from_raw_ptr_opt(value: *mut T::Pointee) -> Option<Self> {
        unsafe { NonNull::new(value).map(|v| Self::from_raw_non_null(v)) }
    }
    pub(crate) fn as_raw_ptr(&self) -> *mut T::Pointee {
        self.as_raw_non_null().as_ptr()
    }
    pub(crate) fn as_slice_of_raw_ptrs<'b>(slice: &'b [Self]) -> &'b [*mut T::Pointee] {
        // Safety: `Ref` is a `#[repr(transparent)]` wrapper around `NonNull<T::Pointee>`
        unsafe { mem::transmute(slice) }
    }
    pub(crate) fn deref(self) -> &'a T::Target
    where
        T: WrapDeref,
    {
        T::deref(self)
    }
    pub(crate) fn to_owned(self) -> Own<T>
    where
        T: WrapClone,
    {
        T::do_clone(self)
    }
    pub(crate) unsafe fn transmute_lifetimes<'other, Other>(self) -> Ref<'other, Other>
    where
        Other: Wrap<Pointee = T::Pointee> + WrapTransmuteLifetimes<T>,
    {
        Ref(self.0, PhantomData)
    }
}

impl<T: WrapEq> Eq for Ref<'_, T> {}

impl<T: WrapEq> PartialEq<Ref<'_, T>> for Ref<'_, T> {
    fn eq(&self, other: &Ref<'_, T>) -> bool {
        WrapEq::eq(*self, *other)
    }
}

impl<T: Wrap> Copy for Ref<'_, T> {}

impl<T: Wrap> Clone for Ref<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: WrapDeref> Deref for Ref<'_, T> {
    type Target = T::Target;

    fn deref(&self) -> &Self::Target {
        Ref::deref(*self)
    }
}

impl<'a, T: WrapOwned> From<&'a Own<T>> for Ref<'a, T> {
    fn from(value: &'a Own<T>) -> Self {
        value.as_ref()
    }
}

impl<T: WrapDeref> fmt::Debug for Ref<'_, T>
where
    T::Target: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

// intentional Send for WrapSync and not WrapSend
unsafe impl<T: WrapSync> Send for Ref<'_, T> {}

unsafe impl<T: WrapSync> Sync for Ref<'_, T> {}

pub(crate) struct LlvmContext<'compiler>(PhantomData<&'compiler ()>);

unsafe impl<'compiler> Wrap for LlvmContext<'compiler> {
    type Pointee = llvm_sys::LLVMContext;
    type PhantomData = ();
}

unsafe impl WrapTransmuteLifetimes<LlvmContext<'_>> for LlvmContext<'_> {}

impl<'compiler> WrapOwned for LlvmContext<'compiler> {
    unsafe fn do_drop(v: NonNull<Self::Pointee>) {
        unsafe { llvm_sys::core::LLVMContextDispose(v.as_ptr()) }
    }
}

impl<'compiler> LlvmContext<'compiler> {
    pub(crate) fn new() -> Own<Self> {
        unsafe { Own::from_raw_ptr(llvm_sys::core::LLVMContextCreate()) }
    }
}

impl fmt::Debug for Ref<'_, LlvmContext<'_>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_raw_non_null().fmt(f)
    }
}

pub(crate) struct LlvmString;

unsafe impl Wrap for LlvmString {
    type Pointee = c_char;
    type PhantomData = ();
}

impl WrapOwned for LlvmString {
    unsafe fn do_drop(v: NonNull<Self::Pointee>) {
        unsafe { llvm_sys::core::LLVMDisposeMessage(v.as_ptr()) }
    }
}

impl WrapClone for LlvmString {
    fn do_clone(v: Ref<'_, Self>) -> Own<Self> {
        unsafe { Own::from_raw_ptr(llvm_sys::core::LLVMCreateMessage(v.as_raw_ptr())) }
    }
}

impl WrapDeref for LlvmString {
    type Target = CStr;

    fn deref<'a>(v: Ref<'a, Self>) -> &'a Self::Target {
        unsafe { CStr::from_ptr(v.as_raw_ptr()) }
    }
}

impl fmt::Display for Ref<'_, LlvmString> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_string_lossy().fmt(f)
    }
}

unsafe impl WrapSend for LlvmString {}
unsafe impl WrapSync for LlvmString {}

impl AsRef<CStr> for Ref<'_, LlvmString> {
    fn as_ref(&self) -> &CStr {
        self
    }
}

impl AsRef<CStr> for Own<LlvmString> {
    fn as_ref(&self) -> &CStr {
        self
    }
}

impl std::error::Error for Own<LlvmString> {}

impl std::error::Error for Ref<'_, LlvmString> {}

impl WrapEq for LlvmString {
    fn eq(a: Ref<'_, Self>, b: Ref<'_, Self>) -> bool {
        *a == *b
    }
}

pub(crate) struct LlvmExecutionEngine;

unsafe impl Wrap for LlvmExecutionEngine {
    type Pointee = llvm_sys::execution_engine::LLVMOpaqueExecutionEngine;
    type PhantomData = ();
}

impl WrapOwned for LlvmExecutionEngine {
    unsafe fn do_drop(v: NonNull<Self::Pointee>) {
        unsafe { llvm_sys::execution_engine::LLVMDisposeExecutionEngine(v.as_ptr()) }
    }
}

impl fmt::Debug for Ref<'_, LlvmExecutionEngine> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_raw_non_null().fmt(f)
    }
}

impl LlvmExecutionEngine {
    pub(crate) unsafe fn create_jit(
        module: Own<LlvmModule<'_, '_>>,
        optimization_level: c_uint,
    ) -> Result<Own<Self>, Own<LlvmString>> {
        let mut execution_engine = ptr::null_mut();
        let mut error = ptr::null_mut();
        unsafe {
            if llvm_sys::execution_engine::LLVMCreateJITCompilerForModule(
                &mut execution_engine,
                module.into_raw_ptr(),
                optimization_level,
                &mut error,
            ) != 0
            {
                Err(Own::from_raw_ptr(error))
            } else {
                Ok(Own::from_raw_ptr(execution_engine))
            }
        }
    }
}

impl<'compiler> Ref<'compiler, LlvmExecutionEngine> {
    pub(crate) unsafe fn add_module<'ctx>(self, module: Own<LlvmModule<'compiler, 'ctx>>) {
        unsafe {
            llvm_sys::execution_engine::LLVMAddModule(self.as_raw_ptr(), module.into_raw_ptr())
        }
    }
    pub(crate) unsafe fn target_machine(self) -> Ref<'compiler, LlvmTargetMachine> {
        unsafe {
            Ref::from_raw_ptr(
                llvm_sys::execution_engine::LLVMGetExecutionEngineTargetMachine(self.as_raw_ptr()),
            )
        }
    }
    pub(crate) unsafe fn target_data(self) -> Ref<'compiler, LlvmTargetData> {
        unsafe {
            Ref::from_raw_ptr(
                llvm_sys::execution_engine::LLVMGetExecutionEngineTargetData(self.as_raw_ptr()),
            )
        }
    }
}

pub(crate) struct LlvmGenericValue;

unsafe impl Wrap for LlvmGenericValue {
    type Pointee = llvm_sys::execution_engine::LLVMOpaqueGenericValue;
    type PhantomData = ();
}

impl WrapOwned for LlvmGenericValue {
    unsafe fn do_drop(v: NonNull<Self::Pointee>) {
        unsafe { llvm_sys::execution_engine::LLVMDisposeGenericValue(v.as_ptr()) }
    }
}

impl fmt::Debug for Ref<'_, LlvmGenericValue> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_raw_non_null().fmt(f)
    }
}

pub(crate) struct LlvmType<'compiler>(PhantomData<&'compiler ()>);

unsafe impl<'compiler> Wrap for LlvmType<'compiler> {
    type Pointee = llvm_sys::LLVMType;

    type PhantomData = &'compiler ();
}

impl WrapEq for LlvmType<'_> {
    fn eq(a: Ref<'_, Self>, b: Ref<'_, Self>) -> bool {
        a.as_raw_non_null() == b.as_raw_non_null()
    }
}

impl Ref<'_, LlvmType<'_>> {
    pub(crate) fn to_string(self) -> Own<LlvmString> {
        unsafe { Own::from_raw_ptr(llvm_sys::core::LLVMPrintTypeToString(self.as_raw_ptr())) }
    }
}

impl fmt::Debug for Ref<'_, LlvmType<'_>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LlvmType")
            .field(&DebugAsDisplay(self.to_string()))
            .finish()
    }
}

pub(crate) struct LlvmValue<'compiler>(PhantomData<&'compiler ()>);

unsafe impl<'compiler> Wrap for LlvmValue<'compiler> {
    type Pointee = llvm_sys::LLVMValue;

    type PhantomData = &'compiler ();
}

impl Ref<'_, LlvmValue<'_>> {
    pub(crate) fn to_string(self) -> Own<LlvmString> {
        unsafe { Own::from_raw_ptr(llvm_sys::core::LLVMPrintValueToString(self.as_raw_ptr())) }
    }
}

impl fmt::Debug for Ref<'_, LlvmValue<'_>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LlvmValue")
            .field(&DebugAsDisplay(self.to_string()))
            .finish()
    }
}

pub(crate) struct LlvmBasicBlock<'compiler>(PhantomData<&'compiler ()>);

unsafe impl<'compiler> Wrap for LlvmBasicBlock<'compiler> {
    type Pointee = llvm_sys::LLVMBasicBlock;

    type PhantomData = &'compiler ();
}

impl Ref<'_, LlvmBasicBlock<'_>> {
    pub(crate) fn to_string(self) -> Own<LlvmString> {
        unsafe {
            Own::from_raw_ptr(llvm_sys::core::LLVMPrintValueToString(
                llvm_sys::core::LLVMBasicBlockAsValue(self.as_raw_ptr()),
            ))
        }
    }
}

impl fmt::Debug for Ref<'_, LlvmBasicBlock<'_>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LlvmBasicBlock")
            .field(&DebugAsDisplay(self.to_string()))
            .finish()
    }
}

pub(crate) struct LlvmModule<'compiler, 'ctx>(PhantomData<&'ctx &'compiler ()>);

unsafe impl<'compiler, 'ctx> Wrap for LlvmModule<'compiler, 'ctx> {
    type Pointee = llvm_sys::LLVMModule;

    type PhantomData = &'ctx &'compiler ();
}

unsafe impl WrapTransmuteLifetimes<LlvmModule<'_, '_>> for LlvmModule<'_, '_> {}

impl<'compiler, 'ctx> LlvmModule<'compiler, 'ctx> {
    pub(crate) fn new(
        context: Ref<'ctx, LlvmContext<'compiler>>,
        name: impl AsRef<CStr>,
    ) -> Own<LlvmModule<'compiler, 'ctx>> {
        unsafe {
            Own::from_raw_ptr(llvm_sys::core::LLVMModuleCreateWithNameInContext(
                name.as_ref().as_ptr(),
                context.as_raw_ptr(),
            ))
        }
    }
    pub(crate) fn parse_ir(
        context: Ref<'ctx, LlvmContext<'compiler>>,
        memory_buffer: Own<LlvmMemoryBuffer<'_>>,
    ) -> Result<Own<LlvmModule<'compiler, 'ctx>>, Own<LlvmString>> {
        let mut module = ptr::null_mut();
        let mut error = ptr::null_mut();
        unsafe {
            if 0 == llvm_sys::ir_reader::LLVMParseIRInContext(
                context.as_raw_ptr(),
                memory_buffer.into_raw_ptr(),
                &mut module,
                &mut error,
            ) {
                Ok(Own::from_raw_ptr(module))
            } else {
                Err(Own::from_raw_ptr(error))
            }
        }
    }
}

impl Ref<'_, LlvmModule<'_, '_>> {
    pub(crate) fn to_string(self) -> Own<LlvmString> {
        unsafe { Own::from_raw_ptr(llvm_sys::core::LLVMPrintModuleToString(self.as_raw_ptr())) }
    }
    pub(crate) fn set_data_layout(self, v: impl AsRef<CStr>) {
        unsafe { llvm_sys::core::LLVMSetDataLayout(self.as_raw_ptr(), v.as_ref().as_ptr()) }
    }
    pub(crate) fn set_target_triple(self, v: impl AsRef<CStr>) {
        unsafe { llvm_sys::core::LLVMSetTarget(self.as_raw_ptr(), v.as_ref().as_ptr()) }
    }
}

impl WrapOwned for LlvmModule<'_, '_> {
    unsafe fn do_drop(v: NonNull<Self::Pointee>) {
        unsafe { llvm_sys::core::LLVMDisposeModule(v.as_ptr()) }
    }
}

struct DebugAsDisplay<T: ?Sized>(T);

impl<T: ?Sized + fmt::Display> fmt::Debug for DebugAsDisplay<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Ref<'_, LlvmModule<'_, '_>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("LlvmModule")
            .field(&DebugAsDisplay(self.to_string()))
            .finish()
    }
}

pub(crate) struct LlvmTargetData;

unsafe impl Wrap for LlvmTargetData {
    type Pointee = llvm_sys::target::LLVMOpaqueTargetData;

    type PhantomData = ();
}

impl WrapOwned for LlvmTargetData {
    unsafe fn do_drop(v: NonNull<Self::Pointee>) {
        unsafe { llvm_sys::target::LLVMDisposeTargetData(v.as_ptr()) }
    }
}

impl LlvmTargetData {
    pub(crate) fn new(layout_string: impl AsRef<CStr>) -> Own<Self> {
        unsafe {
            Own::from_raw_ptr(llvm_sys::target::LLVMCreateTargetData(
                layout_string.as_ref().as_ptr(),
            ))
        }
    }
}

impl Ref<'_, LlvmTargetData> {
    pub(crate) fn to_string(self) -> Own<LlvmString> {
        unsafe {
            Own::from_raw_ptr(llvm_sys::target::LLVMCopyStringRepOfTargetData(
                self.as_raw_ptr(),
            ))
        }
    }
}

impl fmt::Debug for Ref<'_, LlvmTargetData> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("DataLayout")
            .field(&self.to_string())
            .finish()
    }
}

pub(crate) struct LlvmTargetMachine;

unsafe impl Wrap for LlvmTargetMachine {
    type Pointee = llvm_sys::target_machine::LLVMOpaqueTargetMachine;

    type PhantomData = ();
}

impl Ref<'_, LlvmTargetMachine> {
    pub(crate) fn triple(self) -> Own<LlvmString> {
        unsafe {
            Own::from_raw_ptr(llvm_sys::target_machine::LLVMGetTargetMachineTriple(
                self.as_raw_ptr(),
            ))
        }
    }
    pub(crate) fn cpu(self) -> Own<LlvmString> {
        unsafe {
            Own::from_raw_ptr(llvm_sys::target_machine::LLVMGetTargetMachineCPU(
                self.as_raw_ptr(),
            ))
        }
    }
    pub(crate) fn feature_string(self) -> Own<LlvmString> {
        unsafe {
            Own::from_raw_ptr(llvm_sys::target_machine::LLVMGetTargetMachineFeatureString(
                self.as_raw_ptr(),
            ))
        }
    }
}

pub(crate) struct LlvmBuilder<'compiler, 'ctx>(PhantomData<&'ctx &'compiler ()>);

unsafe impl<'compiler, 'ctx> Wrap for LlvmBuilder<'compiler, 'ctx> {
    type Pointee = llvm_sys::LLVMBuilder;

    type PhantomData = &'ctx &'compiler ();
}

impl WrapOwned for LlvmBuilder<'_, '_> {
    unsafe fn do_drop(v: NonNull<Self::Pointee>) {
        unsafe { llvm_sys::core::LLVMDisposeBuilder(v.as_ptr()) }
    }
}

impl<'compiler, 'ctx> LlvmBuilder<'compiler, 'ctx> {
    pub(crate) fn new(context: Ref<'ctx, LlvmContext<'compiler>>) -> Own<Self> {
        unsafe {
            Own::from_raw_ptr(llvm_sys::core::LLVMCreateBuilderInContext(
                context.as_raw_ptr(),
            ))
        }
    }
}

impl fmt::Debug for Ref<'_, LlvmBuilder<'_, '_>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_raw_non_null().fmt(f)
    }
}

pub(crate) struct LlvmMemoryBuffer<'data>(PhantomData<&'data [u8]>);

unsafe impl<'data> Wrap for LlvmMemoryBuffer<'data> {
    type Pointee = llvm_sys::LLVMMemoryBuffer;

    type PhantomData = &'data [u8];
}

impl<'data> WrapOwned for LlvmMemoryBuffer<'data> {
    unsafe fn do_drop(v: NonNull<Self::Pointee>) {
        unsafe { llvm_sys::core::LLVMDisposeMemoryBuffer(v.as_ptr()) }
    }
}

impl<'data> LlvmMemoryBuffer<'data> {
    pub(crate) fn with_memory_range(
        data: &'data [u8],
        name: impl AsRef<CStr>,
        requires_null_terminator: bool,
    ) -> Own<Self> {
        if requires_null_terminator {
            assert_eq!(data.last(), Some(&b'\0'));
        }
        unsafe {
            Own::from_raw_ptr(llvm_sys::core::LLVMCreateMemoryBufferWithMemoryRange(
                data.as_ptr() as *const c_char,
                data.len(),
                name.as_ref().as_ptr(),
                requires_null_terminator as _,
            ))
        }
    }
}

impl<'data> WrapDeref for LlvmMemoryBuffer<'data> {
    type Target = [u8];

    fn deref<'a>(v: Ref<'a, Self>) -> &'a Self::Target {
        unsafe {
            slice::from_raw_parts(
                llvm_sys::core::LLVMGetBufferStart(v.as_raw_ptr()) as *const u8,
                llvm_sys::core::LLVMGetBufferSize(v.as_raw_ptr()),
            )
        }
    }
}
