use crate::block::{Block, BlockPtr};

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Closure(Block);

extern "C" {
    pub fn umber_apply2(fun: Closure, arg1: BlockPtr, arg2: BlockPtr) -> BlockPtr;
}
