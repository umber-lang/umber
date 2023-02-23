use crate::block::BlockPtr;
use libm::sqrt;

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
