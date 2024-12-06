#![allow(static_mut_refs)]

use crate::block::BlockPtr;
use core::{iter, mem};
use heapless::Vec;

// FIXME: Make the effect handler stack thread-local

// FIXME: Where am I going to allocate the stack?
// Maybe we just bite the bullet and use std? Maybe it's fine to do that?
// Or I guess we can treat it like an actual stack and just allocate a bunch of space at
// startup and make sure we don't exceed that. 100 handlers is definitely way too few.

/* FIXME: Think about the design properly: I think we need a tree of handler sections:

   Install handlers: creates a new node as a child of the current node, adds handlers

   Perform effect: search up from the current node to find a matching handler. When found,
   call the function with the current node set to that one. (After calling, reset it.)

   Resume: Works just like a normal function. (Uses the current handlers, not the original
   ones, since it is dynamically rather than lexically scoped.) Calls the continuation.

   Destroy handlers: should be done when we leave a `handle` block. Free the node and move
   to its parent. (But there could be children left, right? Need to deal with that)

   It is a tree because you can install a handler, perform an effect, then go off an
   install more handlers.
*/

static mut HANDLER_STATE: HandlerState = HandlerState::new();

struct HandlerState {
    handler_tree: Vec<HandlerNode, 100>,
    current_node: Option<NodeIndex>,
}

impl HandlerState {
    const fn new() -> Self {
        Self {
            handler_tree: Vec::new(),
            current_node: None,
        }
    }
}

#[derive(Clone, Copy)]
struct NodeIndex(u16);

#[derive(Clone)]
struct HandlerNode {
    handlers: Vec<EffectHandler, 10>,
    parent_node: Option<NodeIndex>,
}

#[derive(Clone, Copy)]
#[repr(C)]
struct EffectHandler {
    effect_op: i64,
    handler_fun: BlockPtr,
}

#[no_mangle]
pub unsafe extern "C" fn umber_install_handlers(n: u16, mut handlers: ...) {
    let handlers: Vec<EffectHandler, 10> = (0..n)
        .map(|_| {
            let effect_op = handlers.arg();
            let handler_fun = handlers.arg();
            EffectHandler {
                effect_op,
                handler_fun: mem::transmute::<u64, BlockPtr>(handler_fun),
            }
        })
        .collect();
    let handler_node = HandlerNode {
        handlers,
        parent_node: HANDLER_STATE.current_node,
    };
    HANDLER_STATE
        .handler_tree
        .push(handler_node)
        .unwrap_or_else(|_| panic!("Too many effect handler nodes, overflowed"))
}

unsafe fn iter_handler_node_parents(
    starting_index: Option<NodeIndex>,
) -> impl Iterator<Item = (NodeIndex, HandlerNode)> {
    let mut next_index = starting_index;
    iter::from_fn(move || {
        next_index.map(|index| {
            let current_node = HANDLER_STATE.handler_tree[index.0 as usize].clone();
            next_index = current_node.parent_node;
            (index, current_node)
        })
    })
}

unsafe fn find_matching_handler(effect_op: i64) -> (NodeIndex, EffectHandler) {
    iter_handler_node_parents(HANDLER_STATE.current_node)
        .find_map(|(index, node)| {
            node.handlers
                .iter()
                .find(|handler| handler.effect_op == effect_op)
                .map(|&handler| (index, handler))
        })
        .expect("Unhandled effect")
}

#[no_mangle]
pub unsafe extern "C" fn umber_perform_effect(effect_op: i64, arg: BlockPtr) {
    let (handler_node_index, handler) = find_matching_handler(effect_op);
    let prev_node = HANDLER_STATE.current_node;
    HANDLER_STATE.current_node = Some(handler_node_index);
    umber_apply1(handler.handler_fun, arg);
    HANDLER_STATE.current_node = prev_node
}

#[no_mangle]
pub unsafe extern "C" fn umber_uninstall_handlers() {
    if HANDLER_STATE.current_node.is_none() {
        panic!("No handlers to uninstall, underflow")
    }
    // FIXME: We want to remove this node and all the children, right?
    // I'm a bit sus of the tree behavior - what if a resume is kept around?
    let mut indexes_to_remove = [false; 100];
    for (index, _node) in iter_handler_node_parents(HANDLER_STATE.current_node) {
        indexes_to_remove[index.0 as usize] = true
    }
    let mut indexes = 0..HANDLER_STATE.handler_tree.len();
    HANDLER_STATE
        .handler_tree
        .retain(|_node| indexes_to_remove[indexes.next().unwrap()])
}

extern "C" {
    fn umber_apply1(fun: BlockPtr, arg: BlockPtr);
}
