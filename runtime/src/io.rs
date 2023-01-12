use crate::block::BlockPtr;
use libc_print::std_name::print;

#[no_mangle]
pub extern "C" fn umber_print_int(x: BlockPtr) {
    print!("{}", x.as_int());
}

#[no_mangle]
pub extern "C" fn umber_print_float(x: BlockPtr) {
    print!("{}", x.as_float());
}

#[no_mangle]
pub extern "C" fn umber_print_bool(x: BlockPtr) {
    let str = if x.as_constant_cnstr().tag() == 0 {
        "False"
    } else {
        "True"
    };
    print!("{}", str);
}

#[no_mangle]
pub extern "C" fn umber_print_string(x: BlockPtr) {
    print!("{}", x.as_str());
}

#[no_mangle]
pub extern "C" fn umber_print_endline(x: BlockPtr) {
    print!("{}\n", x.as_str());
}
