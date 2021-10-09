use crate::gc;
use std::ptr::{copy_nonoverlapping, NonNull};
use std::{mem, slice, str};

#[derive(Clone, Copy)]
union Value {
    pointer: BlockPtr,
    immediate: Immediate,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union BlockPtr {
    block: NonNull<Block>,
    constant_cnstr: ConstantCnstr,
}

const BLOCK_PTR_MASK: u64 = 0x111;

impl BlockPtr {
    fn as_u64(self) -> u64 {
        unsafe { mem::transmute(self) }
    }

    fn get_block(self) -> Option<NonNull<Block>> {
        // Pointers to `Block` always have the last 3 bits set to 0 as it has align 8
        if self.as_u64() & BLOCK_PTR_MASK == 0 {
            unsafe { Some(self.block) }
        } else {
            None
        }
    }

    pub fn as_ptr(self) -> *mut Block {
        self.get_block().unwrap().as_ptr()
    }
}

// Blocks are followed by their data inline (blocks, then immediates)
#[repr(C, align(8))]
pub struct Block {
    tag: u16,
    n_pointers: u16,
    n_immediates: u16,
}

impl Block {
    fn pointers(&self) -> &[BlockPtr] {
        unsafe {
            slice::from_raw_parts(
                (self as *const Self).add(1) as *const BlockPtr,
                self.n_pointers as usize,
            )
        }
    }

    fn immediates(&self) -> &[Immediate] {
        unsafe {
            slice::from_raw_parts(
                (self as *const Self).offset(1) as *const Immediate,
                self.n_immediates as usize,
            )
        }
    }

    pub unsafe fn as_str(&self) -> &str {
        str::from_utf8_unchecked(mem::transmute(self.immediates()))
    }

    // TODO: I think this actually doesn't make sense. We should just malloc the thing
    // first, allowing it to be garbage, and then fill in the fields as we initialize it.
    // This saves having to copy them.
    pub fn new(tag: u16, pointers: &[BlockPtr], immediates: &[Immediate]) -> NonNull<Self> {
        let total_size =
            mem::size_of::<Self>() + mem::size_of_val(pointers) + mem::size_of_val(immediates);
        let heap_block = gc::allocate(total_size);
        let header = Self {
            tag,
            n_pointers: pointers.len() as u16,
            n_immediates: immediates.len() as u16,
        };
        unsafe {
            let heap_block_header = heap_block as *mut Block;
            copy_nonoverlapping(&header, heap_block_header, 1);
            let heap_block_pointers = heap_block_header.add(1) as *mut BlockPtr;
            copy_nonoverlapping(pointers.as_ptr(), heap_block_pointers, pointers.len());
            let heap_block_immediates = heap_block_pointers.add(pointers.len()) as *mut Immediate;
            copy_nonoverlapping(immediates.as_ptr(), heap_block_immediates, immediates.len());
            NonNull::new_unchecked(heap_block_header)
        }
    }
}

// Invariant: `ConstantCnstr` must be odd (least significant bit is 1) to distinguish it
// from `BlockPtr`
#[repr(C)]
#[derive(Clone, Copy)]
struct ConstantCnstr(u64);

#[derive(Clone, Copy)]
pub union Immediate {
    int: i64,
    float: f64,
    char: char,
}
