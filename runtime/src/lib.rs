#![allow(internal_features)]
#![feature(lang_items)]
#![feature(str_internals)]
#![no_std]

mod block;
mod bool;
mod char;
mod closure;
mod fiber;
mod float;
mod gc;
mod int;
mod io;
mod poly;
mod stdlib;
mod string;

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

// The runtime only works on 64-bit
const _: () = assert!(size_of::<u64>() == size_of::<usize>());
