use core::{ffi::c_void, ptr};
use libc::{free, malloc};

/* FIXME: Consider the fiber state.
   - Pointer to parent fibre.
   - List of handlers (effect_op_id, handler address to jump to)
   - Keep the address of the handler list in a register at all times e.g. r14

   When handling:
   - Allocate a new fiber
   - Fill in the header (parent pointer, handlers)
   - Set rsp to new stack location

   When performing:
   - Put perform args in the arg registers for calling a function
   - Jump to handler list in r14 and linear search for matching effect_op_id
   - If found, jump to handler and start executing.
   - If not found, jump to parent fiber and continue.

   When resuming:
   - Set rax with the value to "return" from the perfom call
   - Jump to continuation address

   At main startup:
   - Allocate a toplevel fibre (or might not be necessary?)

*/

// TODO: C function calls need to be done on the regular system stack - we need to save
// rsp and set it to the top of the system stack. That means we also need to store that
// in the runtime state. Use a static struct? Thread safety is sus, we really want this to
// be thread local but Rust's support for that is only in std.
//
// Actually I'm confused why it'd be better to use a pinned register vs a static address.

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
struct EffectOpId(u64);

#[repr(transparent)]
#[derive(Copy, Clone)]
struct Handler(*const c_void);

#[repr(C, align(16))]
struct Fiber {
    parent: *mut Fiber,
    saved_rsp: *const c_void,
    total_size: u64,
    handler_count: u64,
}

unsafe fn get_handler(fiber: *mut Fiber, i: usize) -> (EffectOpId, Handler) {
    let effect_op_id = fiber.add(size_of::<Fiber>() + 2 * i) as *const EffectOpId;
    let handler = fiber.add(size_of::<Fiber>() + 2 * i) as *const Handler;
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

const DEFAULT_FIBER_SIZE: usize = 256 * 8 * 2;

#[no_mangle]
unsafe extern "C" fn umber_fiber_create(parent: *mut Fiber) -> *mut Fiber {
    let fiber = malloc(DEFAULT_FIBER_SIZE) as *mut Fiber;
    // FIXME: Is there some way we can guarantee this? Maybe malloc will always give us
    // aligned data, as long as the fiber size is a multiple of 16?
    if fiber as u64 % 16 != 0 {
        panic!("Fiber address not aligned!")
    }
    (*fiber).parent = parent;
    (*fiber).total_size = DEFAULT_FIBER_SIZE as u64;
    fiber
}

#[no_mangle]
unsafe extern "C" fn umber_fiber_destroy(fiber: *mut Fiber) {
    free(fiber as *mut c_void);
}

#[no_mangle]
unsafe extern "C" fn umber_find_handler(
    mut current_fiber: *mut Fiber,
    effect_op_id: EffectOpId,
    handling_fiber: *mut *mut Fiber,
) -> Handler {
    loop {
        match search_fiber_for_handler(current_fiber, effect_op_id) {
            Some(handler) => {
                (*current_fiber).parent = ptr::null_mut();
                (*handling_fiber) = current_fiber;
                return handler;
            }
            None => current_fiber = (*current_fiber).parent,
        }
    }
}

#[no_mangle]
unsafe extern "C" fn umber_fiber_reparent(
    mut current_fiber: *mut Fiber,
    new_parent_fiber: *mut Fiber,
) {
    loop {
        if (*current_fiber).parent.is_null() {
            (*current_fiber).parent = new_parent_fiber;
            return;
        }
        current_fiber = (*current_fiber).parent;
    }
}
