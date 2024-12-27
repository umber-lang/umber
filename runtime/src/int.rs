use core::fmt::Write;
use core::mem;

use heapless::String;

use crate::block::{Block, BlockPtr, KnownTag};

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
        let x: BlockPtr = unsafe { mem::transmute(x) };
        Self::new(KnownTag::Int as u16, [x])
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
pub extern "C" fn umber_int_div(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_int(x.as_int() / y.as_int())
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

// The largest i64 has 19 decimal digits. The smallest i64 also has a "-" prefix.
const MAX_INT_DIGITS: usize = 20;

#[no_mangle]
pub extern "C" fn umber_int_to_string(x: BlockPtr) -> BlockPtr {
    let mut buf = String::<MAX_INT_DIGITS>::new();
    write!(buf, "{}", x.as_int()).unwrap();
    BlockPtr::new_string(&buf)
}

#[no_mangle]
pub extern "C" fn umber_int_of_string(s: BlockPtr) -> BlockPtr {
    BlockPtr::new_int(s.as_str().parse().expect("failed to parse int"))
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

    #[test]
    fn to_string() {
        unsafe { umber_gc_init() };
        assert_eq!(umber_int_to_string(BlockPtr::new_int(0)).as_str(), "0");
        assert_eq!(umber_int_to_string(BlockPtr::new_int(-1)).as_str(), "-1");
        assert_eq!(umber_int_to_string(BlockPtr::new_int(10)).as_str(), "10");
        assert_eq!(
            umber_int_to_string(BlockPtr::new_int(i64::MIN)).as_str(),
            "-9223372036854775808"
        );
        assert_eq!(
            umber_int_to_string(BlockPtr::new_int(i64::MAX)).as_str(),
            "9223372036854775807"
        );
    }

    #[test]
    fn of_string() {
        unsafe { umber_gc_init() };
        assert_eq!(umber_int_of_string(BlockPtr::new_string("0")).as_int(), 0);
        assert_eq!(umber_int_of_string(BlockPtr::new_string("-1")).as_int(), -1);
        assert_eq!(umber_int_of_string(BlockPtr::new_string("10")).as_int(), 10);
        assert_eq!(
            umber_int_of_string(BlockPtr::new_string("-9223372036854775808")).as_int(),
            i64::MIN
        );
        assert_eq!(
            umber_int_of_string(BlockPtr::new_string("9223372036854775807")).as_int(),
            i64::MAX
        );
    }
}
