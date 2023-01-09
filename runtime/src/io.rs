use libc_print::std_name::{print};
use crate::block::BlockPtr;

#[no_mangle]
pub extern "C" fn umber_print_int(x: BlockPtr) {
    print!("{}", x.as_int());
}

#[no_mangle]
pub extern "C" fn umber_print_float(x: BlockPtr) {
    print!("{}", x.as_float());
}

#[no_mangle]
pub extern "C" fn umber_print_string(x: BlockPtr) {
    print!("{}", x.as_str());
}

#[no_mangle]
pub extern "C" fn umber_print_endline(x: BlockPtr) {
    print!("{}\n", x.as_str());
}
