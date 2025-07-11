use core::{ffi::c_void, ptr};
use libc::{free, malloc};

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
struct EffectOpId(u64);

#[repr(transparent)]
#[derive(Copy, Clone)]
struct Handler(*const c_void);

// TODO: Consider representing fibers as a kind of block and manage them with the gc. If
// the continuation is not called (e.g. with an exception), they will just be leaked. We
// could have the continuation keep them alive. A problem with this is that fibers contain
// code setting up resources which might need to run to avoid leaking said resources.
//
// OCaml solves this by expecting you to explicitly continue or discontinue (continue
// with exception) all your continuations. I think the best way to solve this properly is
// with linear types:
//
// - Continuations should have a linear type so they normally must be resumed exactly once
//   (this also helps enforce that they aren't resumed more than once)
// - Resources also have linear types and we need to prove that they get released
// - Continuations which are never resumed (e.g. exceptions) can be identified and then
//   we need some mechanism of registering basically destructors (Rust-style drop RAII?)
//   => you would be forced to "drop" the continuation which would execute the drop logic
//      for the remaining unreleased resources (we'd need drop glue code for each
//      "perform" position in the program.)

#[repr(C)]
struct Fiber {
    parent: *mut Fiber,
    saved_rbp: *const c_void,
    saved_rsp: *const c_void,
    total_size: u64,
    handler_count: u64,
}

impl Fiber {
    const HEADER_SIZE: usize = size_of::<Self>();
}

const _: () = assert!(Fiber::HEADER_SIZE == 5 * 8);

unsafe fn get_handler(fiber: *mut Fiber, i: usize) -> (EffectOpId, Handler) {
    let header_end = (fiber as *const c_void).add(Fiber::HEADER_SIZE);
    let effect_op_id = (header_end as *const EffectOpId).add(2 * i);
    let handler = (header_end as *const Handler).add(2 * i + 1);
    (*effect_op_id, *handler)
}

unsafe fn search_fiber_for_handler(fiber: *mut Fiber, effect_op_id: EffectOpId) -> Option<Handler> {
    let mut i: u64 = 0;
    while i < (*fiber).handler_count {
        let (handler_op_id, handler) = get_handler(fiber, i as usize);
        if handler_op_id == effect_op_id {
            return Some(handler);
        }
        i += 1;
    }
    None
}

// TODO: This needs to be divisble by 16. Pick a big number here so we always have enough
// and don't have to implement stack overflow checks and reallocation yet. Should do that
// properly eventually.
const DEFAULT_FIBER_SIZE: usize = 256 * 8 * 2 * 16 * 2;

#[no_mangle]
unsafe extern "C" fn umber_fiber_create(parent: *mut Fiber) -> *mut Fiber {
    // Malloc will always give us 16-byte aligned data as long as the size requested is at
    // least 16 bytes.
    let fiber = malloc(DEFAULT_FIBER_SIZE) as *mut Fiber;
    assert!(fiber as usize % 16 == 0);
    (*fiber).parent = parent;
    (*fiber).total_size = DEFAULT_FIBER_SIZE as u64;
    fiber
}

#[no_mangle]
unsafe extern "C" fn umber_fiber_destroy(fiber: *mut Fiber) {
    free(fiber as *mut c_void);
}

#[no_mangle]
unsafe extern "C" fn umber_fiber_find_handler_and_detach(
    mut current_fiber: *mut Fiber,
    effect_op_id: EffectOpId,
    new_fiber: *mut *mut Fiber,
) -> Handler {
    loop {
        match search_fiber_for_handler(current_fiber, effect_op_id) {
            Some(handler) => {
                *new_fiber = (*current_fiber).parent;
                (*current_fiber).parent = ptr::null_mut();
                return handler;
            }
            None => current_fiber = (*current_fiber).parent,
        }
    }
}

#[no_mangle]
unsafe extern "C" fn umber_fiber_reparent(
    mut child_fiber: *mut Fiber,
    new_parent_fiber: *mut Fiber,
) {
    loop {
        if (*child_fiber).parent.is_null() {
            (*child_fiber).parent = new_parent_fiber;
            return;
        }
        child_fiber = (*child_fiber).parent;
    }
}
