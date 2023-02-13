use crate::block::{Block, BlockPtr};

#[no_mangle]
pub extern "C" fn umber_int_add(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    Block::new_int(x.as_int() + y.as_int())
}

#[no_mangle]
pub extern "C" fn umber_int_sub(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    Block::new_int(x.as_int() - y.as_int())
}

#[no_mangle]
pub extern "C" fn umber_int_mul(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    Block::new_int(x.as_int() * y.as_int())
}

#[no_mangle]
pub extern "C" fn umber_int_pow(base: BlockPtr, exp: BlockPtr) -> BlockPtr {
    Block::new_int(base.as_int().pow(exp.as_int() as u32))
}
