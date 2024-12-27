use core::mem;

use crate::block::{Block, BlockPtr, KnownTag};

impl Block {
    pub fn as_char(self) -> char {
        self.expect_tag(KnownTag::Char);
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

#[cfg(test)]
mod test {
    use crate::{block::BlockPtr, gc::umber_gc_init};

    #[test]
    fn roundtrip() {
        unsafe { umber_gc_init() };
        assert_eq!(BlockPtr::new_char('a').as_char(), 'a');
        assert_eq!(BlockPtr::new_char('福').as_char(), '福')
    }
}
