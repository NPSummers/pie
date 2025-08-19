use std::{collections::HashMap, rc::Rc};

use crate::typecheck::Type;
use inkwell::{
    context::Context,
    types::{FunctionType, IntType, PointerType, VoidType},
    AddressSpace,
};
mod arithmetic;
mod io;
mod iter;
mod list;
mod map;
mod rc;
mod string;

#[derive(Debug, Clone)]
pub struct StdFunction {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub path: Vec<&'static str>,
}

#[derive(Debug, Clone)]
pub struct NativeFunction<'ctx> {
    pub addr: usize,
    pub func_type: FunctionType<'ctx>,
    pub native_name: &'static str,
    pub std: Option<Rc<StdFunction>>,
}

pub struct Registry<'ctx> {
    ctx: &'ctx Context,
    functions: HashMap<&'static str, Rc<NativeFunction<'ctx>>>,
    std_paths: HashMap<Vec<&'static str>, Rc<NativeFunction<'ctx>>>,
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
                let f = crate::piestd::StdFunction {
                    params,
                    return_type,
                    path,
                };
                std = Some(::std::rc::Rc::new(f));
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
            functions: HashMap::new(),
            std_paths: HashMap::new(),
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
        let func = Rc::new(func);
        let mut old = None;
        if let Some(std) = &func.std {
            old = self.std_paths.insert(std.path.clone(), func.clone());
        }
        old = self.functions.insert(func.native_name, func).or(old);
        if let Some(old) = old {
            panic!("Attempted to overwrite native function {old:?} with new function")
        };
    }
    pub fn register_builtins(&mut self) {
        rc::register(self);
        string::register(self);
        arithmetic::register(self);
        map::register(self);
        list::register(self);
        io::register(self);
        iter::register(self);
    }
    pub fn functions(&self) -> impl Iterator<Item = &NativeFunction<'ctx>> {
        self.functions.values().map(|v| v.as_ref())
    }
    pub fn get_by_native_name<'a>(&'a self, name: &str) -> Option<&'a NativeFunction<'ctx>> {
        self.functions.get(name).map(Rc::as_ref)
    }
    pub fn get_by_std_path<'a>(&'a self, path: &[&'a str]) -> Option<&'a NativeFunction<'ctx>> {
        self.std_paths.get(path).map(Rc::as_ref)
    }
}
