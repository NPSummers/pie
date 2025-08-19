use inkwell::{context::Context, types::BasicTypeEnum, AddressSpace};

use crate::runtime::{GcBox, GcRef};

pub trait LLVMTypeEquivalent {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx>;
}

impl LLVMTypeEquivalent for i64 {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        ctx.i64_type().into()
    }
}

impl LLVMTypeEquivalent for u64 {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        ctx.i64_type().into()
    }
}

impl LLVMTypeEquivalent for f64 {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        ctx.f64_type().into()
    }
}

impl<T> LLVMTypeEquivalent for *const T {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        ctx.ptr_type(AddressSpace::from(0)).into()
    }
}

impl<T> LLVMTypeEquivalent for *mut T {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        ctx.ptr_type(AddressSpace::from(0)).into()
    }
}

impl LLVMTypeEquivalent for GcBox {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        ctx.ptr_type(AddressSpace::from(0)).into()
    }
}

impl LLVMTypeEquivalent for Option<GcBox> {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        ctx.ptr_type(AddressSpace::from(0)).into()
    }
}

impl LLVMTypeEquivalent for GcRef<'_> {
    fn as_llvm_type<'ctx>(ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        ctx.ptr_type(AddressSpace::from(0)).into()
    }
}
