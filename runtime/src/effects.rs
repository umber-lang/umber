use crate::block::BlockPtr;
use heapless::Vec;

// FIXME: Make the effect handler stack thread-local

// FIXME: Where am I going to allocate the stack?
// Maybe we just bite the bullet and use std? Maybe it's fine to do that?
// Or I guess we can treat it like an actual stack and just allocate a bunch of space at
// startup and make sure we don't exceed that. 100 handlers is definitely way too few.
static mut HANDLER_STACK: Vec<EffectHandler, 100> = Vec::new();

struct EffectHandler {
    effect_op: i64,
    handler_fun: BlockPtr,
}

#[no_mangle]
pub unsafe extern "C" fn umber_push_handler(effect_op: i64, handler_fun: BlockPtr) {
    HANDLER_STACK.push(EffectHandler {
        effect_op,
        handler_fun,
    })
}

#[no_mangle]
pub unsafe extern "C" fn umber_perform_effect(effect_op: i64) {
    HANDLER_STACK
        .iter()
        .rev()
        .find(|handler| handler.effect_op == effect_op)
}

#[no_mangle]
pub unsafe extern "C" fn umber_pop_handler() {
    HANDLER_STACK.pop();
}
