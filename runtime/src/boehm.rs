use libc::{c_void, size_t};

// TODO: Use cbindgen to generate these bindings

#[link(name = "gc", kind = "static")]
extern "C" {
    pub fn GC_malloc(nbytes: size_t) -> *mut c_void;
    pub fn GC_malloc_atomic(nbytes: size_t) -> *mut c_void;

    // fn GC_base(arg1: *mut c_void) -> *mut c_void;
}

// TODO: use for differentiating closures and function pointers
// pub unsafe fn is_heap_pointer(ptr: *mut c_void) -> bool {
//     GC_base(ptr).is_null()
// }
