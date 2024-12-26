use core::{ptr, slice};

use libc_print::std_name::print;

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
pub extern "C" fn umber_print_bool(x: BlockPtr) {
    let str = if x.as_bool() { "True" } else { "False" };
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

#[no_mangle]
pub extern "C" fn umber_read_line() -> BlockPtr {
    let mut buffer = ptr::null_mut();
    let mut size = 0;
    let bytes_read = unsafe { libc::getline(&raw mut buffer, &raw mut size, stdin) };
    let result: Option<BlockPtr> = if bytes_read == -1 {
        // TODO: Assuming this means EOF - should check for errors in errno
        None
    } else {
        let bytes = unsafe { slice::from_raw_parts(buffer as *mut u8, bytes_read as usize) };
        // TODO: This should be a proper error (exception), not a runtime panic
        let s = core::str::from_utf8(bytes).expect("umber_read_line: invalid utf8");
        let s = s
            .strip_suffix('\n')
            .unwrap_or(s)
            .strip_suffix("\n\r")
            .unwrap_or(s);
        Some(BlockPtr::new_string(s))
    };
    result.into()
}

extern "C" {
    // The Rust libc bindings don't expose stdin, so we have to declare it ourselves.
    // TODO: This doesn't work on all platforms, see here:
    // https://github.com/rust-lang/libc/issues/2778
    pub static mut stdin: *mut libc::FILE;
}
