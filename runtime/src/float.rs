use crate::block::{Block, BlockPtr, KnownTag};
use core::mem;
use libm::sqrt;

impl Block {
    pub fn as_float(self) -> f64 {
        self.expect_tag(KnownTag::Float);
        unsafe { mem::transmute(self.get_field(0)) }
    }
}

impl BlockPtr {
    pub fn as_float(self) -> f64 {
        self.as_block().as_float()
    }

    pub fn new_float(x: f64) -> BlockPtr {
        unsafe { Self::new(KnownTag::Float, [mem::transmute(x)]) }
    }
}

#[no_mangle]
pub extern "C" fn umber_float_add(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(x.as_float() + y.as_float())
}

#[no_mangle]
pub extern "C" fn umber_float_sub(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(x.as_float() - y.as_float())
}

#[no_mangle]
pub extern "C" fn umber_float_mul(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(x.as_float() * y.as_float())
}

#[no_mangle]
pub extern "C" fn umber_float_sqrt(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(sqrt(x.as_float()))
}
