use crate::boehm;
use core::ptr::{copy_nonoverlapping, NonNull};
use core::{mem, slice, str};

#[repr(C)]
#[derive(Clone, Copy)]
pub union BlockPtr {
    block: NonNull<Block>,
    pub constant_cnstr: ConstantCnstr,
}

const BLOCK_PTR_MASK: u64 = 0x1;

impl BlockPtr {
    fn is_block(&self) -> bool {
        let value: u64 = unsafe { mem::transmute(*self) };
        value & BLOCK_PTR_MASK == 0
    }

    pub fn classify<'a>(&'a self) -> Value<'a> {
        if self.is_block() {
            unsafe {
                match KnownTag::try_from((*self.block.as_ptr()).tag) {
                    Ok(KnownTag::Int) => Value::Int((*self.block.as_ptr()).as_int()),
                    Ok(KnownTag::Float) => Value::Float((*self.block.as_ptr()).as_float()),
                    Ok(KnownTag::String) => Value::String((*self.block.as_ptr()).as_str()),
                    Err(()) => Value::OtherBlock(self.block),
                }
            }
        } else {
            unsafe { Value::ConstantCnstr(self.constant_cnstr) }
        }
    }

    pub fn as_block(&self) -> NonNull<Block> {
        if self.is_block() {
            unsafe { self.block }
        } else {
            panic!(
                "Expected block but got constant constructor: {:x?}",
                self as *const Self
            )
        }
    }

    pub fn as_constant_cnstr(&self) -> ConstantCnstr {
        if self.is_block() {
            panic!(
                "Expected constant constructor but got block: {:x?}",
                self as *const Self
            )
        } else {
            unsafe { self.constant_cnstr }
        }
    }

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

// Blocks consist of this one-word header followed by their fields inline.
// Rust's dynamically-sized types don't let us do this without fat pointers getting
// involved, and we want to store the block length inline, so we just manage the pointers
// ourselves.
#[repr(C, align(8))]
pub struct Block {
    pub tag: u16,
    pub len: u16,
}

// This must be kept in sync with the same definitions in codegen.ml.
#[repr(u16)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum KnownTag {
    Int = 0x8001,
    // Char = 0x8002,
    Float = 0x8003,
    String = 0x8004,
}

impl KnownTag {
    const NO_SCAN_TAG: u16 = 0x8000;

    pub fn no_scan(&self) -> bool {
        *self as u16 >= Self::NO_SCAN_TAG
    }
}

impl TryFrom<u16> for KnownTag {
    type Error = ();

    fn try_from(tag: u16) -> Result<Self, Self::Error> {
        match tag {
            0x8001 => Ok(KnownTag::Int),
            0x8003 => Ok(KnownTag::Float),
            0x8004 => Ok(KnownTag::String),
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
    OtherBlock(NonNull<Block>),
}

impl Block {
    pub fn fields(&self) -> &[BlockPtr] {
        unsafe {
            slice::from_raw_parts(
                (self as *const Self as *const BlockPtr).add(1),
                self.len as usize,
            )
        }
    }

    fn get_field(&self, index: u16) -> BlockPtr {
        unsafe { *(self as *const Self as *const BlockPtr).add(index as usize + 1) }
    }

    // These kinds of runtime checks shouldn't be needed if the compiler produced correct
    // code, but is helpful for debugging the compiler.
    // TODO: Put this stuff behind some kind of debug cfg
    fn expect_tag(&self, tag: KnownTag) {
        if self.tag.try_into() != Ok(tag) {
            panic!(
                "Expected block with tag {:?} but got tag {:?}",
                tag, self.tag
            )
        }
    }

    pub fn as_int(&self) -> i64 {
        self.expect_tag(KnownTag::Int);
        unsafe { mem::transmute(self.get_field(0)) }
    }

    // TODO: use
    // pub fn as_char(&self) -> char {
    //     self.expect_tag(Tag::Char);
    //     let u64: u64 = unsafe { mem::transmute(self.first_field()) };
    //     char::from_u32(u64 as u32).expect("Invalid utf8 char")
    // }

    pub fn as_float(&self) -> f64 {
        self.expect_tag(KnownTag::Float);
        unsafe { mem::transmute(self.get_field(0)) }
    }

    fn string_len(&self) -> usize {
        let last_byte =
            unsafe { *(self as *const Block as *const u8).add(8 * (self.len as usize + 1) - 1) };
        (self.len as usize) * 8 - (last_byte as usize) - 1
    }

    pub fn as_str(&self) -> &str {
        self.expect_tag(KnownTag::String);
        unsafe {
            let bytes =
                slice::from_raw_parts((self as *const Self).add(1) as *const u8, self.string_len());
            str::from_utf8_unchecked(bytes)
        }
    }

    fn new<const N: usize>(tag: KnownTag, fields: [BlockPtr; N]) -> BlockPtr {
        let len = fields.len() as u16;
        unsafe {
            let nbytes = 8 * (len + 1) as usize;
            let block = if tag.no_scan() {
                boehm::GC_malloc_atomic(nbytes) as *mut Self
            } else {
                boehm::GC_malloc(nbytes) as *mut Self
            };
            (*block).tag = tag as u16;
            (*block).len = len;
            copy_nonoverlapping(fields.as_ptr(), block.add(1) as *mut BlockPtr, len as usize);
            BlockPtr {
                block: NonNull::new_unchecked(block),
            }
        }
    }

    pub fn new_int(x: i64) -> BlockPtr {
        unsafe { Self::new(KnownTag::Int, [mem::transmute(x)]) }
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

impl From<bool> for ConstantCnstr {
    fn from(value: bool) -> Self {
        Self::new(value.into())
    }
}
