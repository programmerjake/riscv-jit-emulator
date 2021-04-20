// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use super::ContextRef;
use core::{
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    num::NonZeroUsize,
    ptr::NonNull,
};

/// Declare a struct that can be used with [`ContextRef::type_for`].
///
/// Example:
/// ```
/// # use riscv_jit_emulator::struct_with_ir_type;
/// struct_with_ir_type! {
///     #[repr(C)]
///     /// My struct.
///     #[derive(Copy, Clone)]
///     pub struct MyStruct<'a> {
///         pub a: u32,
///         /// My Float
///         float_field: f32,
///         ptr: &'a MyStruct<'a>,
///     }
/// }
/// ```
///
/// It automatically checks for fields that need padding (and are hence potentially confusing to use with [`crate::backend`]).
/// ```compile_fail
/// # use riscv_jit_emulator::struct_with_ir_type;
/// struct_with_ir_type! {
///     #[repr(C)]
///     struct MyErroneousStruct {
///         a: u8,
///         // compile error because padding would be required here
///         b: u64,
///     }
/// }
/// ```
///
/// You can fix the above compile error by explicitly inserting a padding field:
/// ```
/// # use riscv_jit_emulator::struct_with_ir_type;
/// struct_with_ir_type! {
///     #[repr(C)]
///     struct MyFixedStruct {
///         a: u8,
///         _padding: [u8; 7], // explicit padding field
///         b: u64,
///     }
/// }
/// ```
#[macro_export]
macro_rules! struct_with_ir_type {
    (
        #[repr(C)]
        $(#[$type_meta:meta])*
        $type_vis:vis struct $type_name:ident $(<$($lifetimes:lifetime),*>)? {
            $(
                $(#[$field_meta:meta])*
                $field_vis:vis $field_name:ident: $field_type:ty,
            )*
        }
    ) => {
        #[repr(C)]
        $(#[$type_meta])*
        $type_vis struct $type_name $(<$($lifetimes),*>)? {
            $(
                $(#[$field_meta])*
                $field_vis $field_name: $field_type,
            )*
        }

        unsafe impl $(<$($lifetimes),*>)? $crate::backend::types::TypeFor for $type_name $(<$($lifetimes),*>)? {
            fn type_for<C: $crate::backend::ContextRef>(context: C) -> C::Type {
                let fields = [
                    $($crate::backend::ContextRef::type_for::<$field_type>(context)),*
                ];
                $crate::backend::ContextRef::struct_type(context, &fields).into()
            }

            const LAYOUT_SUMMARY: $crate::backend::types::TypeLayoutSummary = {
                let builder = $crate::backend::types::StructTypeLayoutBuilder::new();
                $(
                    let builder = builder.field(<$field_type as $crate::backend::types::TypeFor>::LAYOUT_SUMMARY);
                    let builder = $crate::backend::types::StructTypeLayoutError::const_unwrap(builder);
                )*
                builder.finish()
            };
        }

        const _: $crate::backend::types::TypeLayoutSummary = <$type_name as $crate::backend::types::TypeFor>::LAYOUT_SUMMARY;
    };
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct TypeLayoutSummary {
    pub size: usize,
    /// Minimum alignment guaranteed to work cross-platform.
    /// In particular, `safe_alignment == size` for all pointer, float, and integer types.
    pub safe_alignment: NonZeroUsize,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum StructTypeLayoutError {
    FieldNotAlignedToSafeOffset { field_index: usize },
    SizeOverflow,
}

impl StructTypeLayoutError {
    #[track_caller]
    pub const fn const_unwrap(
        result: Result<StructTypeLayoutBuilder, Self>,
    ) -> StructTypeLayoutBuilder {
        #![allow(unconditional_panic)] // the whole point
        match result {
            Ok(v) => return v,
            Err(Self::FieldNotAlignedToSafeOffset { field_index }) => {
                ["Field not aligned to safe offset -- you can fix this by adding a padding \
                  Field before this field in your `struct_with_ir_type!` struct. \
                  See `struct_with_ir_type`'s documentation for details"; 0][field_index];
            }
            Err(Self::SizeOverflow) => {
                ["struct size overflow"][1];
            }
        }
        loop {
            [0; 0][0];
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum ScalarTypeLayoutError {
    EmptyType,
    SizeIsNotPowerOfTwo,
}

impl ScalarTypeLayoutError {
    #[track_caller]
    pub const fn const_unwrap(result: Result<TypeLayoutSummary, Self>) -> TypeLayoutSummary {
        #![allow(unconditional_panic)] // the whole point
        match result {
            Ok(v) => return v,
            Err(Self::EmptyType) => {
                ["scalar type must not be empty"][1];
            }
            Err(Self::SizeIsNotPowerOfTwo) => {
                ["scalar type's size must be a power of two"][1];
            }
        }
        [][0]
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum ArrayTypeLayoutError {
    SizeOverflow,
}

impl ArrayTypeLayoutError {
    #[track_caller]
    pub const fn const_unwrap(result: Result<TypeLayoutSummary, Self>) -> TypeLayoutSummary {
        #![allow(unconditional_panic)] // the whole point
        match result {
            Ok(v) => return v,
            Err(Self::SizeOverflow) => {
                ["array size overflow"][1];
            }
        }
        [][0]
    }
}

macro_rules! unwrap_or {
    ($expr:expr, $fallback:expr) => {
        match $expr {
            Some(v) => v,
            None => $fallback,
        }
    };
}

#[derive(Debug)]
#[must_use]
pub struct StructTypeLayoutBuilder {
    layout: TypeLayoutSummary,
    last_field_offset: usize,
    last_field_index: usize,
}

impl StructTypeLayoutBuilder {
    pub const fn new() -> Self {
        Self {
            layout: TypeLayoutSummary::EMPTY,
            last_field_offset: 0,
            last_field_index: 0,
        }
    }
    pub const fn last_field_offset(&self) -> usize {
        self.last_field_offset
    }
    pub const fn last_field_index(&self) -> usize {
        self.last_field_index
    }
    pub const fn field(self, field: TypeLayoutSummary) -> Result<Self, StructTypeLayoutError> {
        let Self {
            mut layout,
            last_field_offset: _,
            mut last_field_index,
        } = self;
        if layout.size % field.safe_alignment.get() != 0 {
            return Err(StructTypeLayoutError::FieldNotAlignedToSafeOffset {
                field_index: last_field_index,
            });
        }
        let last_field_offset = layout.size;
        layout.size = unwrap_or!(
            layout.size.checked_add(field.size),
            return Err(StructTypeLayoutError::SizeOverflow)
        );
        if field.safe_alignment.get() > layout.safe_alignment.get() {
            layout.safe_alignment = field.safe_alignment;
        }
        last_field_index += 1;
        Ok(Self {
            layout,
            last_field_offset,
            last_field_index,
        })
    }
    pub const fn finish(self) -> TypeLayoutSummary {
        self.layout
    }
}

impl TypeLayoutSummary {
    pub const EMPTY: Self = Self {
        size: 0,
        safe_alignment: unsafe { NonZeroUsize::new_unchecked(1) },
    };
    pub const fn for_scalar(size: usize) -> Result<Self, ScalarTypeLayoutError> {
        let safe_alignment = match NonZeroUsize::new(size) {
            Some(v) if size.is_power_of_two() => v,
            Some(_) => return Err(ScalarTypeLayoutError::SizeIsNotPowerOfTwo),
            None => return Err(ScalarTypeLayoutError::EmptyType),
        };
        Ok(Self {
            size,
            safe_alignment,
        })
    }
    pub const fn for_array(element: Self, length: usize) -> Result<Self, ArrayTypeLayoutError> {
        let size = unwrap_or!(
            element.size.checked_mul(length),
            return Err(ArrayTypeLayoutError::SizeOverflow)
        );
        Ok(Self {
            size,
            safe_alignment: element.safe_alignment,
        })
    }
}

/// Get the corresponding IR type for a Rust type. Prefer using [`ContextRef::type_for`].
///
/// Not implemented for `bool` since LLVM's `i1` is not the same as Rust's `bool`.
///
/// Use [`struct_with_ir_type`] to implement `TypeFor` for a struct.
pub unsafe trait TypeFor {
    fn type_for<C: ContextRef>(context: C) -> C::Type;
    const LAYOUT_SUMMARY: TypeLayoutSummary;
}

macro_rules! impl_scalar_type_for {
    ($ty:ty => $fn:ident) => {
        unsafe impl TypeFor for $ty {
            fn type_for<C: ContextRef>(context: C) -> C::Type {
                context.$fn().into()
            }
            const LAYOUT_SUMMARY: TypeLayoutSummary = ScalarTypeLayoutError::const_unwrap(
                TypeLayoutSummary::for_scalar(core::mem::size_of::<Self>()),
            );
        }
    };
}

impl_scalar_type_for!(u8 => i8_type);
impl_scalar_type_for!(i8 => i8_type);
impl_scalar_type_for!(u16 => i16_type);
impl_scalar_type_for!(i16 => i16_type);
impl_scalar_type_for!(u32 => i32_type);
impl_scalar_type_for!(i32 => i32_type);
impl_scalar_type_for!(u64 => i64_type);
impl_scalar_type_for!(i64 => i64_type);
impl_scalar_type_for!(u128 => i128_type);
impl_scalar_type_for!(i128 => i128_type);
impl_scalar_type_for!(usize => isize_type);
impl_scalar_type_for!(isize => isize_type);
impl_scalar_type_for!(f32 => f32_type);
impl_scalar_type_for!(f64 => f64_type);

unsafe impl<T: TypeFor, const N: usize> TypeFor for [T; N] {
    fn type_for<C: ContextRef>(context: C) -> C::Type {
        context.array_type(context.type_for::<T>(), N).into()
    }
    const LAYOUT_SUMMARY: TypeLayoutSummary =
        ArrayTypeLayoutError::const_unwrap(TypeLayoutSummary::for_array(T::LAYOUT_SUMMARY, N));
}

macro_rules! impl_ptr_type_for {
    (impl<$target:ident> TypeFor for $ty:ty) => {
        unsafe impl<$target: TypeFor> TypeFor for $ty {
            fn type_for<C: ContextRef>(context: C) -> C::Type {
                context.ptr_type(context.type_for::<T>()).into()
            }
            const LAYOUT_SUMMARY: TypeLayoutSummary = <usize as TypeFor>::LAYOUT_SUMMARY;
        }
    };
}

macro_rules! impl_non_null_ptr_type_for {
    (impl<$target:ident> TypeFor for $ty:ty) => {
        impl_ptr_type_for!(impl<$target> TypeFor for $ty);
        impl_ptr_type_for!(impl<$target> TypeFor for Option<$ty>);
    };
}

impl_non_null_ptr_type_for!(impl<T> TypeFor for NonNull<T>);
impl_non_null_ptr_type_for!(impl<T> TypeFor for &'_ T);
impl_non_null_ptr_type_for!(impl<T> TypeFor for &'_ mut T);
impl_ptr_type_for!(impl<T> TypeFor for *mut T);
impl_ptr_type_for!(impl<T> TypeFor for *const T);

macro_rules! impl_fn_type_for {
    ($($arg_name:ident: $arg_ty:ident),*) => {
        unsafe impl<$($arg_ty: TypeFor),*> TypeFor for unsafe extern "C" fn($($arg_ty),*) {
            fn type_for<C: ContextRef>(context: C) -> C::Type {
                context.fn_ptr_type(&[$(context.type_for::<$arg_ty>()),*], None).into()
            }
            const LAYOUT_SUMMARY: TypeLayoutSummary = <usize as TypeFor>::LAYOUT_SUMMARY;
        }

        unsafe impl<Ret: TypeFor$(, $arg_ty: TypeFor)*> TypeFor for unsafe extern "C" fn($($arg_ty),*) -> Ret {
            fn type_for<C: ContextRef>(context: C) -> C::Type {
                context.fn_ptr_type(&[$(context.type_for::<$arg_ty>()),*], Some(context.type_for::<Ret>())).into()
            }
            const LAYOUT_SUMMARY: TypeLayoutSummary = <usize as TypeFor>::LAYOUT_SUMMARY;
        }
    };
}

macro_rules! impl_fns_type_for {
    ($($first_arg_name:ident: $first_arg_ty:ident$(, $arg_name:ident: $arg_ty:ident)*)?) => {
        $(impl_fns_type_for!($($arg_name: $arg_ty),*);)?
        impl_fn_type_for!($($first_arg_name: $first_arg_ty$(, $arg_name: $arg_ty)*)?);
    };
}

impl_fns_type_for!(
    arg1: Arg1,
    arg2: Arg2,
    arg3: Arg3,
    arg4: Arg4,
    arg5: Arg5,
    arg6: Arg6,
    arg7: Arg7,
    arg8: Arg8,
    arg9: Arg9,
    arg10: Arg10,
    arg11: Arg11,
    arg12: Arg12
);

unsafe impl<T: TypeFor> TypeFor for Cell<T> {
    fn type_for<C: ContextRef>(context: C) -> C::Type {
        T::type_for(context)
    }

    const LAYOUT_SUMMARY: TypeLayoutSummary = T::LAYOUT_SUMMARY;
}

unsafe impl<T: TypeFor> TypeFor for UnsafeCell<T> {
    fn type_for<C: ContextRef>(context: C) -> C::Type {
        T::type_for(context)
    }

    const LAYOUT_SUMMARY: TypeLayoutSummary = T::LAYOUT_SUMMARY;
}

unsafe impl<T: ?Sized> TypeFor for PhantomData<T> {
    fn type_for<C: ContextRef>(context: C) -> C::Type {
        context.struct_type(&[]).into()
    }

    const LAYOUT_SUMMARY: TypeLayoutSummary = TypeLayoutSummary::EMPTY;
}

#[cfg(test)]
mod test {
    use super::{StructTypeLayoutBuilder, StructTypeLayoutError, TypeFor};

    struct_with_ir_type! {
        #[repr(C)]
        /// My struct.
        #[derive(Copy, Clone)]
        pub(crate) struct MyStruct<'a> {
            pub a: u32,
            /// My Float
            float_field: f32,
            ptr: &'a MyStruct<'a>,
        }
    }

    #[test]
    fn test_struct_layout() {
        let builder = StructTypeLayoutBuilder::new();
        let builder = builder.field(<u8 as TypeFor>::LAYOUT_SUMMARY).unwrap();
        assert_eq!(
            builder.field(<u64 as TypeFor>::LAYOUT_SUMMARY).unwrap_err(),
            StructTypeLayoutError::FieldNotAlignedToSafeOffset { field_index: 1 }
        );
    }
}
