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

// TODO: Handle panics in arithmetic operations by converting them into exceptions

#[no_mangle]
pub extern "C" fn umber_int_pow(base: BlockPtr, exp: BlockPtr) -> BlockPtr {
    BlockPtr::new_int(base.as_int().pow(exp.as_int() as u32))
}

#[no_mangle]
pub extern "C" fn umber_int_rem(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_int(x.as_int() % y.as_int())
}

#[no_mangle]
pub extern "C" fn umber_int_mod(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    // Euclidean modulus, copied from the implementation in the Rust standard library:
    // https://doc.rust-lang.org/stable/src/core/num/int_macros.rs.html#2135
    let x = x.as_int();
    let y = y.as_int();
    let r = x % y;
    let r = if r < 0 {
        r.wrapping_add(y.wrapping_abs())
    } else {
        r
    };
    BlockPtr::new_int(r)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{block::BlockPtr, gc::umber_gc_init};

    #[test]
    fn basic_arithmetic() {
        unsafe { umber_gc_init() };
        assert_eq!(
            umber_int_add(BlockPtr::new_int(2), BlockPtr::new_int(3)).as_int(),
            5
        );
        assert_eq!(
            umber_int_sub(BlockPtr::new_int(0), BlockPtr::new_int(1)).as_int(),
            -1
        );
        assert_eq!(
            umber_int_mul(BlockPtr::new_int(5), BlockPtr::new_int(7)).as_int(),
            35
        );
        assert_eq!(
            umber_int_pow(BlockPtr::new_int(2), BlockPtr::new_int(6)).as_int(),
            64
        );
    }
}
