use core::arch::asm;
use core::mem;

use crate::block::{Block, BlockPtr};

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Closure(Block);

// Call a closure, passing the closure itself and then 2 arguments.
pub unsafe fn umber_apply2(closure: Closure, arg1: BlockPtr, arg2: BlockPtr) -> BlockPtr {
    let fun_ptr: u64 = mem::transmute(closure.0.get_field(0));
    let closure = closure.0.as_ptr();
    let arg1: u64 = mem::transmute(arg1);
    let arg2: u64 = mem::transmute(arg2);
    let mut result: u64;
    // We have to spill and restore rbx because it's used internally by LLVM.
    // Then, because we pushed to the stack, it will be misaligned for the call, so we
    // need to adjust rsp further.
    asm!(
        "push rbx",
        "sub rsp, 8",
        "mov rbx, {arg1}",
        "call {fun_ptr}",
        "add rsp, 8",
        "pop rbx",
        fun_ptr = in(reg) fun_ptr,
        inout("rax") closure => result,
        arg1 = in(reg) arg1,
        inout("rdi") arg2 => _,
        lateout("rsi") _,
        lateout("rdx") _,
        lateout("rcx") _,
        lateout("r8") _,
        lateout("r9")  _,
        lateout("r10") _,
        lateout("r11") _,
        lateout("r12") _,
        lateout("r13") _,
        lateout("r14") _,
        lateout("r15") _,
    );
    mem::transmute(result)
}
