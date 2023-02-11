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

impl Gc {
    pub unsafe fn get() -> &'static mut Self {
        if GC.heap.is_null() {
            panic!("GC was not initialized! `umber_gc_init` must be called before use")
        } else {
            &mut GC
        }
    }

    pub unsafe fn alloc(&mut self, nbytes: usize) -> *mut u8 {
        if self.next_free + nbytes >= self.heap_capacity {
            panic!("Out of memory (the \"garbage collection\" part of the garbage collector is not implemented yet)")
        } else {
            let result = self.heap.add(self.next_free);
            self.next_free += nbytes;
            result
        }
    }

    pub unsafe fn is_on_heap(&self, x: *const c_void) -> bool {
        ((x as usize) >= (self.heap as usize))
            && ((x as usize) < (self.heap.add(self.heap_capacity) as usize))
    }
}

#[no_mangle]
pub unsafe extern "C" fn umber_gc_is_on_heap(x: *const c_void) -> bool {
    Gc::get().is_on_heap(x)
}
