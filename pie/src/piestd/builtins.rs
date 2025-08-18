use crate::typecheck::Type;
use inkwell::{
    context::Context,
    types::{FunctionType, IntType, PointerType, VoidType},
    AddressSpace,
};
mod arithmetic;
mod int;
mod io;
mod list;
mod map;
mod rc;
mod string;

pub struct StdFunction {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub path: Vec<&'static str>,
}

pub struct NativeFunction<'ctx> {
    pub addr: usize,
    pub func_type: FunctionType<'ctx>,
    pub native_name: &'static str,
    pub std: Option<StdFunction>,
}

pub struct Registry<'ctx> {
    ctx: &'ctx Context,
    functions: Vec<NativeFunction<'ctx>>,
}

macro_rules! pie_native_fn {
    (unsafe $name:ident($($param:ident : $ty:ty),*) $(pie $path:literal[$($piety:ident),*] $(=> $pieret:ident)?)? $(-> $ret:ty)? $body:block ) => {
    ::paste::paste! {
        fn [<$name _register>]<'ctx>(reg: &mut crate::piestd::builtins::Registry<'ctx>) {
            #[allow(unused_imports)]
            use ::inkwell::types::{BasicType, BasicMetadataTypeEnum};
            #[allow(unused_imports)]
            use crate::piestd::llvmtypes::LLVMTypeEquivalent;
            #[allow(unused)]
            let ret_type = reg.void_type();
            $(let ret_type = <$ret as LLVMTypeEquivalent>::as_llvm_type(reg.ctx);)?
            let params: &[BasicMetadataTypeEnum] = &[
                $(
                    <$ty as LLVMTypeEquivalent>::as_llvm_type(reg.ctx).into()
                ),*
            ];
            let func_type = ret_type.fn_type(params, false);
            let native_name = stringify!($name);
            let addr = $name as usize;
            #[allow(unused)]
            let mut std = None;
            $(
                let path = $path.split("::").collect();
                let params = vec![$(crate::typecheck::Type::$piety),*];
                #[allow(unused)]
                let return_type = crate::typecheck::Type::Void;
                $(let return_type = crate::typecheck::Type::$pieret;)?
                std = Some(crate::piestd::StdFunction {
                    params,
                    return_type,
                    path,
                });
            )?
            let func = crate::piestd::builtins::NativeFunction {
                native_name,
                addr,
                func_type,
                std
            };
            reg.register(func);
        }
    }

        #[no_mangle]
        pub unsafe extern "C" fn $name($($param: $ty),*) $(-> $ret)? $body
    };
    ($name:ident($($param:ident : $ty:ty),*) $(pie $path:literal[$($piety:ident),*] $(=> $pieret:ident)?)? $(-> $ret:ty)? $body:block ) => {
    ::paste::paste! {
        fn [<$name _register>]<'ctx>(reg: &mut crate::piestd::builtins::Registry<'ctx>) {
            #[allow(unused_imports)]
            use ::inkwell::types::{BasicType, BasicMetadataTypeEnum};
            #[allow(unused_imports)]
            use crate::piestd::llvmtypes::LLVMTypeEquivalent;
            #[allow(unused)]
            let ret_type = reg.void_type();
            $(let ret_type = <$ret as LLVMTypeEquivalent>::as_llvm_type(reg.ctx);)?
            let params: &[BasicMetadataTypeEnum] = &[
                $(
                    <$ty as LLVMTypeEquivalent>::as_llvm_type(reg.ctx).into()
                ),*
            ];
            let func_type = ret_type.fn_type(params, false);
            let native_name = stringify!($name);
            let addr = $name as usize;
            #[allow(unused)]
            let mut std = None;
            $(
                let path = $path.split("::").collect();
                let params = vec![$(crate::typecheck::Type::$piety),*];
                #[allow(unused)]
                let return_type = crate::typecheck::Type::Void;
                $(let return_type = crate::typecheck::Type::$pieret;)?
                std = Some(crate::piestd::StdFunction {
                    params,
                    return_type,
                    path,
                });
            )?
            let func = crate::piestd::builtins::NativeFunction {
                native_name,
                addr,
                func_type,
                std
            };
            reg.register(func);
        }
    }

        #[no_mangle]
        pub extern "C" fn $name($($param: $ty),*) $(-> $ret)? $body
    };
}
pub(crate) use pie_native_fn;

impl<'ctx> Registry<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        Registry {
            functions: Vec::new(),
            ctx,
        }
    }
    pub fn ptr_type(&self) -> PointerType<'ctx> {
        self.ctx.ptr_type(AddressSpace::from(0))
    }
    pub fn i64_type(&self) -> IntType<'ctx> {
        self.ctx.i64_type()
    }
    pub fn void_type(&self) -> VoidType<'ctx> {
        self.ctx.void_type()
    }
    pub fn register(&mut self, func: NativeFunction<'ctx>) {
        self.functions.push(func);
    }
    pub fn register_builtins(&mut self) {
        string::register(self);
        arithmetic::register(self);
        map::register(self);
        list::register(self);
        int::register(self);
        io::register(self);
    }
    pub fn functions(&self) -> impl Iterator<Item = &NativeFunction<'ctx>> {
        self.functions.iter()
    }
}
