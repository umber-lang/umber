use std::ptr::NonNull;
use std::{mem, slice, str};

#[repr(C)]
#[derive(Clone, Copy)]
pub union BlockPtr {
    block: NonNull<Block>,
    constant_cnstr: ConstantCnstr,
}

const BLOCK_PTR_MASK: u64 = 0x1;

impl BlockPtr {
    fn as_u64(&self) -> u64 {
        unsafe { mem::transmute(self) }
    }

    fn get_block(&self) -> Option<NonNull<Block>> {
        if self.as_u64() & BLOCK_PTR_MASK == 0 {
            unsafe { Some(self.block) }
        } else {
            None
        }
    }

    pub fn as_int(&self) -> i64 {
        unsafe { self.get_block().unwrap().as_ref().as_int() }
    }
    pub fn as_float(&self) -> f64 {
        unsafe { self.get_block().unwrap().as_ref().as_float() }
    }

    pub fn as_str(&self) -> &str {
        unsafe { self.get_block().unwrap().as_ref().as_str() }
    }
}

// Blocks consist of this header followed by their fields inline
#[repr(C, align(8))]
struct Block {
    tag: u16,
    len: u16,
}

impl Block {
    fn first_field(&self) -> BlockPtr {
        unsafe { *(self as *const Self as *const BlockPtr).add(1) }
    }

    fn fields(&self) -> &[BlockPtr] {
        unsafe {
            slice::from_raw_parts(
                (self as *const Self as *const BlockPtr).add(1),
                self.len as usize,
            )
        }
    }

    fn as_int(&self) -> i64 {
        unsafe { mem::transmute(self.first_field()) }
    }

    fn as_float(&self) -> f64 {
        unsafe { mem::transmute(self.first_field()) }
    }

    unsafe fn as_str(&self) -> &str {
        str::from_utf8_unchecked(mem::transmute(self.fields()))
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
struct ConstantCnstr(u64);
