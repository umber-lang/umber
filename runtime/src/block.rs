use crate::gc::Gc;
use core::ptr::{copy_nonoverlapping, NonNull};
use core::{mem, slice, str};

// A `BlockPtr` is either a 63-bit integer `ConstantCnstr` (constant constructor) or a
// pointer to a block, which is a heap-allocated array of `BlockPtr`, of which the first
// element is a `BlockHeader`.
#[repr(C)]
#[derive(Clone, Copy)]
pub union BlockPtr {
    block: Block,
    pub constant_cnstr: ConstantCnstr,
}

const BLOCK_PTR_MASK: u64 = 0x1;

impl BlockPtr {
    pub fn is_block(self) -> bool {
        let value: u64 = unsafe { mem::transmute(self) };
        value & BLOCK_PTR_MASK == 0
    }

    pub fn classify<'a>(self) -> Value<'a> {
        if self.is_block() {
            unsafe {
                match KnownTag::try_from(self.block.header().tag) {
                    Ok(KnownTag::Int) => Value::Int(self.block.expect_int()),
                    Ok(KnownTag::Float) => Value::Float(self.block.expect_float()),
                    Ok(KnownTag::String) => Value::String(self.block.expect_str()),
                    Ok(KnownTag::Forward) | Err(()) => Value::OtherBlock(self.block),
                }
            }
        } else {
            unsafe { Value::ConstantCnstr(self.constant_cnstr) }
        }
    }

    pub fn as_block(self) -> Option<Block> {
        if self.is_block() {
            unsafe { Some(self.block) }
        } else {
            None
        }
    }

    pub fn expect_block(self) -> Block {
        unsafe {
            if self.is_block() {
                self.block
            } else {
                panic!(
                    "Expected block but got constant constructor: {:x?}",
                    self.constant_cnstr
                )
            }
        }
    }

    pub fn expect_constant_cnstr(self) -> ConstantCnstr {
        unsafe {
            if self.is_block() {
                panic!(
                    "Expected constant constructor but got block: {:x?}",
                    self.block
                )
            } else {
                self.constant_cnstr
            }
        }
    }

    pub fn new<const N: usize>(tag: u16, fields: [BlockPtr; N]) -> BlockPtr {
        let len: u16 = fields.len().try_into().unwrap();
        unsafe {
            Self::new_with_initializer(tag, len, |block| {
                copy_nonoverlapping(
                    fields.as_ptr(),
                    block.0.as_ptr().add(1) as *mut BlockPtr,
                    len as usize,
                )
            })
        }
    }

    pub unsafe fn new_with_initializer(
        tag: u16,
        len: u16,
        initialize: impl FnOnce(Block),
    ) -> BlockPtr {
        let n_bytes = 8 * (len + 1) as usize;
        let header = Gc::get().alloc(n_bytes) as *mut BlockHeader;
        (*header).tag = tag;
        (*header).len = len;
        let block = Block(NonNull::new_unchecked(header as *mut BlockPtr));
        initialize(block);
        BlockPtr { block }
    }
}

// This must be kept in sync with the same definitions in cnstr_tag.ml.
#[repr(u16)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(enum_iterator::Sequence))]
pub enum KnownTag {
    Int = 0x8001,
    // Char = 0x8002,
    Float = 0x8003,
    String = 0x8004,
    Forward = 0x9000,
}

impl TryFrom<u16> for KnownTag {
    type Error = ();

    fn try_from(tag: u16) -> Result<Self, Self::Error> {
        match tag {
            0x8001 => Ok(KnownTag::Int),
            0x8003 => Ok(KnownTag::Float),
            0x8004 => Ok(KnownTag::String),
            0x9000 => Ok(KnownTag::Forward),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Value<'a> {
    Int(i64),
    Float(f64),
    String(&'a str),
    ConstantCnstr(ConstantCnstr),
    OtherBlock(Block),
}

#[repr(C, align(8))]
#[derive(Copy, Clone)]
pub struct BlockHeader {
    pub tag: u16,
    pub len: u16,
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct Block(NonNull<BlockPtr>);

impl Block {
    pub fn as_ptr(self) -> *mut BlockPtr {
        self.0.as_ptr()
    }

    pub fn new(p: NonNull<BlockPtr>) -> Self {
        Self(p)
    }

    pub fn header<'a>(self) -> &'a BlockHeader {
        unsafe { &*(self.as_ptr() as *const BlockHeader) }
    }

    pub fn header_mut(self) -> *mut BlockHeader {
        self.as_ptr() as *mut BlockHeader
    }

    // This kind of lifetime is safe because the GC will ensure all lifetimes are as long
    // as needed.
    pub fn fields<'a>(self) -> &'a [BlockPtr] {
        unsafe { slice::from_raw_parts(self.as_ptr().add(1), self.header().len as usize) }
    }

    pub unsafe fn get_field(self, index: u16) -> BlockPtr {
        *self.as_ptr().add(index as usize + 1)
    }

    pub unsafe fn set_field(self, index: u16, value: BlockPtr) {
        *self.as_ptr().add(index as usize + 1) = value;
    }

    pub fn classify<'a>(self) -> Value<'a> {
        let ptr = BlockPtr { block: self };
        ptr.classify()
    }

    /// Set the tag of this block to [Forward] and have it point to the new block.
    /// Used by the garbage collector.
    pub fn forward(self, new_block: Self) {
        unsafe {
            (*self.header_mut()).tag = KnownTag::Forward as u16;
            self.set_field(0, BlockPtr { block: new_block })
        }
    }

    // These kinds of runtime checks shouldn't be needed if the compiler produced correct
    // code, but is helpful for debugging the compiler.
    // TODO: Put this stuff behind some kind of debug cfg
    pub fn expect_tag(self, tag: KnownTag) {
        if self.header().tag.try_into() != Ok(tag) {
            panic!(
                "Expected block with tag {:?} but got tag {:?}",
                tag,
                self.header().tag
            )
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstantCnstr(u64);

impl ConstantCnstr {
    pub fn new(tag: u64) -> Self {
        Self((tag << 1) | BLOCK_PTR_MASK)
    }

    pub fn tag(&self) -> u64 {
        self.0 >> 1
    }
}

#[cfg(test)]
mod test {
    use super::KnownTag;

    #[test]
    fn all_known_tags_roundtrip() {
        for tag in enum_iterator::all::<KnownTag>() {
            assert_eq!(Ok(tag), KnownTag::try_from(tag as u16))
        }
    }
}
