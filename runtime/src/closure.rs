use core::{ffi::c_void, mem};

use crate::{
    block::{Block, BlockPtr},
    gc,
};

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Closure(Block);

// This is a copy of the logic used in generating the umber_applyN functions in LLVM
// TODO: Maybe better to call the functions in the runtime rather than copying here?
// The advantage of having separate implementations is they don't have to use the C ABI
macro_rules! apply_impl {
    ( $name:ident, $($arg:ident),* ) => {
        pub unsafe fn $name(self, $($arg: BlockPtr),*) -> BlockPtr {
            // If the pointer is on the heap, do a closure call. Otherwise, do a regular
            // function call.
            if gc::is_on_heap(gc::get(), mem::transmute::<Self, *const c_void>(self)) {
                let fun: fn(env: Closure, $($arg: BlockPtr),*) -> BlockPtr = mem::transmute(self.0.get_field(0));
                fun(self, $($arg),*)
            } else {
                let fun: fn($($arg: BlockPtr),*) -> BlockPtr = mem::transmute(self);
                fun($($arg),*)
            }
        }
    };
}

impl Closure {
    apply_impl!(apply2, arg1, arg2);
}
