use crate::block::BlockPtr;
use libm::sqrt;

#[no_mangle]
pub extern "C" fn umber_float_sqrt(x: BlockPtr) -> BlockPtr {
    BlockPtr::new_float(sqrt(x.as_float()))
}
