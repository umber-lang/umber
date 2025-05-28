use crate::block::{Block, BlockPtr, KnownTag};
use core::mem;

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
        let x: BlockPtr = unsafe { mem::transmute(x) };
        Self::new(KnownTag::Float as u16, [x])
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
    BlockPtr::new_float(libm::sqrt(x.as_float()))
}

#[no_mangle]
pub extern "C" fn umber_float_abs(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(libm::fabs(x.as_float()))
}

#[no_mangle]
pub extern "C" fn umber_float_sin(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(libm::sin(x.as_float()))
}

#[no_mangle]
pub extern "C" fn umber_float_cos(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(libm::cos(x.as_float()))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{block::BlockPtr, gc::umber_gc_init};

    #[test]
    fn basic_arithmetic() {
        unsafe { umber_gc_init() };
        assert_eq!(
            umber_float_add(BlockPtr::new_float(2.), BlockPtr::new_float(3.)).as_float(),
            5.
        );
        assert_eq!(
            umber_float_sub(BlockPtr::new_float(0.), BlockPtr::new_float(1.)).as_float(),
            -1.
        );
        assert_eq!(
            umber_float_mul(BlockPtr::new_float(5.), BlockPtr::new_float(7.)).as_float(),
            35.
        );
    }

    // This test doesn't work under MIRI (at least on x86) because libm::sqrt uses inline
    // assembly in its implementation and MIRI doesn't support this.
    #[cfg(not(miri))]
    #[test]
    fn sqrt() {
        assert_eq!(umber_float_sqrt(BlockPtr::new_float(4.)).as_float(), 2.);
    }
}
