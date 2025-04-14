use core::mem;

use crate::block::{Block, BlockPtr};

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Closure(Block);

// TODO: use core::arch::asm to translate to the Umber calling convention when that
// becomes different from C's.
pub unsafe extern "C" fn umber_apply2(fun: Closure, arg1: BlockPtr, arg2: BlockPtr) -> BlockPtr {
    let fun_ptr: extern "C" fn(BlockPtr, BlockPtr) -> BlockPtr = mem::transmute(fun.0.get_field(0));
    fun_ptr(arg1, arg2)
}
