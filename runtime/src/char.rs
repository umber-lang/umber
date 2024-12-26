use core::mem;

use crate::block::{Block, BlockPtr, KnownTag};

impl Block {
    pub fn as_char(self) -> char {
        self.expect_tag(KnownTag::Int);
        unsafe {
            let c: u64 = mem::transmute(self.get_field(0));
            char::from_u32_unchecked(c as u32)
        }
    }
}

impl BlockPtr {
    pub fn as_char(self) -> char {
        self.as_block().as_char()
    }

    pub fn new_char(c: char) -> BlockPtr {
        let c: BlockPtr = unsafe { mem::transmute(c as u64) };
        Self::new(KnownTag::Char as u16, [c])
    }
}
