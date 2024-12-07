#![allow(static_mut_refs)]

use core::ffi::c_void;
use core::{panic, ptr};

const DEFAULT_HEAP_SIZE: usize = 256 * 1000;

pub struct Gc {
    next_free: usize,
    heap_capacity: usize,
    heap: *mut u8,
}

static mut GC: Gc = Gc {
    next_free: 0,
    heap_capacity: 0,
    heap: ptr::null_mut(),
};

#[no_mangle]
pub unsafe extern "C" fn umber_gc_init() {
    GC = Gc {
        next_free: 0,
        heap_capacity: DEFAULT_HEAP_SIZE,
        heap: libc::malloc(DEFAULT_HEAP_SIZE) as *mut u8,
    }
}

pub unsafe fn get() -> *mut Gc {
    if GC.heap.is_null() {
        panic!("GC was not initialized! `umber_gc_init` must be called before use")
    } else {
        &raw mut GC
    }
}

pub unsafe fn alloc(gc: *mut Gc, n_bytes: usize) -> *mut u8 {
    if (*gc).next_free + n_bytes >= (*gc).heap_capacity {
        panic!("Out of memory (the \"garbage collection\" part of the garbage collector is not implemented yet)")
    } else {
        let result = (*gc).heap.add((*gc).next_free);
        (*gc).next_free += n_bytes;
        result
    }
}

pub unsafe fn is_on_heap(gc: *mut Gc, x: *const c_void) -> bool {
    ((x as usize) >= ((*gc).heap as usize))
        && ((x as usize) < ((*gc).heap.add((*gc).heap_capacity) as usize))
}

#[no_mangle]
pub unsafe extern "C" fn umber_gc_is_on_heap(x: *const c_void) -> bool {
    is_on_heap(get(), x)
}

#[no_mangle]
pub unsafe extern "C" fn umber_gc_alloc(n_bytes: usize) -> *mut u8 {
    alloc(get(), n_bytes)
}
