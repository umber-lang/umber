// TODO: I copied this code from the boehm_gc crate, which relies on rust nightly features
// and is currently broken. I should probably just use that crate when it's fixed.

use libc::{c_void, size_t};

#[link(name = "gc")]
extern "C" {
    pub fn GC_malloc(nbytes: size_t) -> *mut c_void;
}

#[inline]
pub fn allocate(size: usize) -> *mut u8 {
    unsafe { GC_malloc(size as size_t) as *mut u8 }
}
