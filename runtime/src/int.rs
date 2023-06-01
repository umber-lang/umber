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

    fn check(f: extern "C" fn(BlockPtr, BlockPtr) -> BlockPtr, x: i64, y: i64, expect: i64) {
        assert_eq!(
            f(BlockPtr::new_int(x), BlockPtr::new_int(y)).as_int(),
            expect
        );
    }

    #[test]
    fn basic_arithmetic() {
        unsafe { umber_gc_init() };
        check(umber_int_add, 2, 3, 5);
        check(umber_int_sub, 0, 1, -1);
        check(umber_int_mul, 5, 7, 35);
        check(umber_int_pow, 2, 6, 64);
    }

    #[test]
    fn rem_and_mod() {
        unsafe { umber_gc_init() };

        check(umber_int_rem, 5, 4, 1);
        check(umber_int_rem, -5, 4, -1);
        check(umber_int_rem, 5, -4, 1);
        check(umber_int_rem, -5, -4, -1);

        check(umber_int_mod, 5, 4, 1);
        check(umber_int_mod, -5, 4, 3);
        check(umber_int_mod, 5, -4, 1);
        check(umber_int_mod, -5, -4, 3);
    }
}
