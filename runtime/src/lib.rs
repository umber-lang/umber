#![feature(lang_items)]
#![no_std]

use core::panic::PanicInfo;
use libc::exit;
use libc_print::std_name::eprintln;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    eprintln!("Umber runtime panic: {}", info);
    unsafe { exit(42) }
}

#[lang = "eh_personality"]
extern "C" fn eh_personality() {}

mod block;
mod int;
mod io;
