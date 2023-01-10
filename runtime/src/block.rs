use core::ptr::{copy_nonoverlapping, NonNull};
use core::{mem, slice, str};

#[repr(C)]
#[derive(Clone, Copy)]
pub union BlockPtr {
    block: NonNull<Block>,
    constant_cnstr: ConstantCnstr,
}

const BLOCK_PTR_MASK: u64 = 0x1;

impl BlockPtr {
    fn is_block(&self) -> bool {
        (self as *const Self as u64) & BLOCK_PTR_MASK == 0
    }

    pub fn as_block(&self) -> NonNull<Block> {
        if self.is_block() {
            unsafe { self.block }
        } else {
            panic!("Expected block but got constant constructor")
        }
    }

    // TODO: use
    // pub fn as_constant_cnstr(&self) -> ConstantCnstr {
    //     if self.is_block() {
    //         panic!("Expected constant constructor but got block")
    //     } else {
    //         unsafe { self.constant_cnstr }
    //     }
    // }

    pub fn as_int(&self) -> i64 {
        unsafe { self.as_block().as_ref().as_int() }
    }

    pub fn as_float(&self) -> f64 {
        unsafe { self.as_block().as_ref().as_float() }
    }

    pub fn as_str(&self) -> &str {
        unsafe { self.as_block().as_ref().as_str() }
    }
}

// Blocks consist of this one-word header followed by their fields inline
// Rust's dynamically-sized types don't let us do this without fat pointers getting
// involved, and we want to store the block length inline, so we just malloc and manage
// the pointers ourselves.
#[repr(C, align(8))]
pub struct Block {
    tag: Tag,
    len: u16,
}

// This must be kept in sync with the same definitions in codegen.ml.
#[repr(u16)]
#[derive(Debug, PartialEq, Eq)]
pub enum Tag {
    Int = 0x8001,
    // Char = 0x8002,
    Float = 0x8003,
    String = 0x8004,
}

impl Block {
    fn get_field(&self, index: u16) -> BlockPtr {
        unsafe { *(self as *const Self as *const BlockPtr).add(index as usize + 1) }
    }

    // These kinds of runtime checks shouldn't be needed if the compiler produced correct
    // code, but is helpful for debugging the compiler.
    // TODO: Put this stuff behind some kind of debug cfg
    fn expect_tag(&self, tag: Tag) {
        if self.tag != tag {
            panic!(
                "Expected block with tag {:?} but got tag {:?}",
                tag, self.tag
            )
        }
    }

    pub fn as_int(&self) -> i64 {
        self.expect_tag(Tag::Int);
        unsafe { mem::transmute(self.get_field(0)) }
    }

    // TODO: use
    // pub fn as_char(&self) -> char {
    //     self.expect_tag(Tag::Char);
    //     let u64: u64 = unsafe { mem::transmute(self.first_field()) };
    //     char::from_u32(u64 as u32).expect("Invalid utf8 char")
    // }

    pub fn as_float(&self) -> f64 {
        self.expect_tag(Tag::Float);
        unsafe { mem::transmute(self.get_field(0)) }
    }

    fn string_len(&self) -> usize {
        let last_byte =
            unsafe { *(self as *const Block as *const u8).add(8 * (self.len as usize + 1)) };
        (self.len as usize) * 8 - (last_byte as usize) - 1
    }

    pub fn as_str(&self) -> &str {
        self.expect_tag(Tag::String);
        unsafe {
            let bytes =
                slice::from_raw_parts((self as *const Self).add(1) as *const u8, self.string_len());
            str::from_utf8_unchecked(bytes)
        }
    }

    // Just malloc and leak memory for now. We can implement GC later.
    fn new<const N: usize>(tag: Tag, fields: [BlockPtr; N]) -> BlockPtr {
        let len = fields.len() as u16;
        unsafe {
            let block = libc::malloc(8 * (len + 1) as usize) as *mut Block;
            (*block).tag = tag;
            (*block).len = len;
            copy_nonoverlapping(fields.as_ptr(), block.add(1) as *mut BlockPtr, len as usize);
            BlockPtr {
                block: NonNull::new_unchecked(block),
            }
        }
    }

    pub fn new_int(x: i64) -> BlockPtr {
        unsafe { Self::new(Tag::Int, [mem::transmute(x)]) }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ConstantCnstr(u64);
