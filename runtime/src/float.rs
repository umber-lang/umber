use crate::block::{Block, BlockPtr, KnownTag};
use core::mem;

impl Block {
    pub fn expect_float(self) -> f64 {
        self.expect_tag(KnownTag::Float);
        unsafe { mem::transmute(self.get_field(0)) }
    }
}

impl BlockPtr {
    pub fn expect_float(self) -> f64 {
        self.expect_block().expect_float()
    }

    pub fn new_float(x: f64) -> BlockPtr {
        unsafe { Self::new(KnownTag::Float as u16, [mem::transmute(x)]) }
    }
}

#[no_mangle]
pub extern "C" fn umber_float_add(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(x.expect_float() + y.expect_float())
}

#[no_mangle]
pub extern "C" fn umber_float_sub(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(x.expect_float() - y.expect_float())
}

#[no_mangle]
pub extern "C" fn umber_float_mul(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(x.expect_float() * y.expect_float())
}

#[no_mangle]
pub extern "C" fn umber_float_sqrt(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(libm::sqrt(x.expect_float()))
}

#[no_mangle]
pub extern "C" fn umber_float_abs(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(libm::fabs(x.expect_float()))
}

#[no_mangle]
pub extern "C" fn umber_float_sin(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(libm::sin(x.expect_float()))
}

#[no_mangle]
pub extern "C" fn umber_float_cos(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(libm::cos(x.expect_float()))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{block::BlockPtr, gc::umber_gc_init};

    #[test]
    fn basic_arithmetic() {
        unsafe { umber_gc_init() };
        assert_eq!(
            umber_float_add(BlockPtr::new_float(2.), BlockPtr::new_float(3.)).expect_float(),
            5.
        );
        assert_eq!(
            umber_float_sub(BlockPtr::new_float(0.), BlockPtr::new_float(1.)).expect_float(),
            -1.
        );
        assert_eq!(
            umber_float_mul(BlockPtr::new_float(5.), BlockPtr::new_float(7.)).expect_float(),
            35.
        );
        assert_eq!(umber_float_sqrt(BlockPtr::new_float(4.)).expect_float(), 2.);
    }
}
