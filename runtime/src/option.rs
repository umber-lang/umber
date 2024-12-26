use crate::block::{BlockPtr, ConstantCnstr};

pub fn some(x: BlockPtr) -> BlockPtr {
    BlockPtr::new(0, [x])
}

pub const NONE: BlockPtr = BlockPtr {
    constant_cnstr: ConstantCnstr::new(0),
};
