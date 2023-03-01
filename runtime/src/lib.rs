#![feature(lang_items)]
#![no_std]

#[cfg(not(test))]
mod no_std_setup {
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
}

#[cfg(not(test))]
pub use no_std_setup::*;

mod block;
mod float;
mod gc;
mod int;
mod io;
mod poly;
mod string;
