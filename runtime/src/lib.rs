#![allow(internal_features)]
#![feature(lang_items)]
#![feature(c_variadic)]
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

mod block;
mod bool;
mod effects;
mod float;
mod gc;
mod int;
mod io;
mod poly;
mod string;
