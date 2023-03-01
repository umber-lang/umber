use crate::block::{Block, BlockPtr, KnownTag};
use core::mem;

impl Block {
    pub fn as_int(self) -> i64 {
        self.expect_tag(KnownTag::Int);
        unsafe { mem::transmute(self.get_field(0)) }
    }
}

impl BlockPtr {
    pub fn as_int(self) -> i64 {
        self.as_block().as_int()
    }

    pub fn new_int(x: i64) -> BlockPtr {
        unsafe { Self::new(KnownTag::Int, [mem::transmute(x)]) }
    }
}

#[no_mangle]
pub extern "C" fn umber_int_add(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_int(x.as_int() + y.as_int())
}

#[no_mangle]
pub extern "C" fn umber_int_sub(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_int(x.as_int() - y.as_int())
}

#[no_mangle]
pub extern "C" fn umber_int_mul(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_int(x.as_int() * y.as_int())
}

#[no_mangle]
pub extern "C" fn umber_int_pow(base: BlockPtr, exp: BlockPtr) -> BlockPtr {
    BlockPtr::new_int(base.as_int().pow(exp.as_int() as u32))
}

#[cfg(test)]
mod test {
    use crate::{block::BlockPtr, gc::umber_gc_init};

    use super::umber_int_add;

    #[test]
    fn basic_arithmetic() {
        unsafe { umber_gc_init() };
        assert_eq!(
            umber_int_add(BlockPtr::new_int(2), BlockPtr::new_int(3)).as_int(),
            5
        );
        assert_eq!(
            umber_int_add(BlockPtr::new_int(0), BlockPtr::new_int(1)).as_int(),
            1
        );
    }
}
