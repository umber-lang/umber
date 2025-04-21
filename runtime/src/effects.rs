use core::ffi::c_void;
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

// FIXME: cleanup
//#[repr(transparent)]
//#[derive(Copy, Clone, PartialEq, Eq)]
//struct EffectOpId(u64);

// #[repr(transparent)]
// #[derive(Copy, Clone)]
// struct Handler(*const c_void);

#[repr(C, align(16))]
struct Fiber {
    parent: *const Fiber,
    total_size: u64,
    handler_count: u64,
}

// FIXME: cleanup
// unsafe fn get_handler(fiber: *const Fiber, i: usize) -> (EffectOpId, Handler) {
//     let effect_op_id = fiber.add(size_of::<Fiber>() + 2 * i) as *const EffectOpId;
//     let handler = fiber.add(size_of::<Fiber>() + 2 * i) as *const Handler;
//     (*effect_op_id, *handler)
// }

// unsafe fn search_fiber_for_handler(
//     fiber: *const Fiber,
//     effect_op_id: EffectOpId,
// ) -> Option<Handler> {
//     let mut i: u64 = 0;
//     while i < (*fiber).handler_count {
//         let (handler_op_id, handler) = get_handler(fiber, i as usize);
//         if handler_op_id == effect_op_id {
//             return Some(handler);
//         }
//         i += 1;
//     }
//     None
// }

const DEFAULT_FIBER_SIZE: usize = 256 * 8 * 2;

#[no_mangle]
unsafe extern "C" fn umber_fiber_create(parent: *const Fiber) -> *mut Fiber {
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

// FIXME: cleanup
// #[no_mangle]
// unsafe extern "C" fn umber_find_handler(effect_op_id: EffectOpId) -> Handler {
//     let mut fiber = CURRENT_FIBER as *const Fiber;
//     loop {
//         match search_fiber_for_handler(fiber, effect_op_id) {
//             Some(handler) => return handler,
//             None => fiber = (*fiber).parent,
//         }
//     }
// }
