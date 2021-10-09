use crate::types::BlockPtr;

#[no_mangle]
pub extern "C" fn print_int(n: i64) {
    print!("{}", n);
}

#[no_mangle]
pub extern "C" fn print_float(x: f64) {
    print!("{}", x);
}

#[no_mangle]
pub extern "C" fn print_string(str: BlockPtr) {
    let str = unsafe { (*str.as_ptr()).as_str() };
    print!("{}", str);
}
