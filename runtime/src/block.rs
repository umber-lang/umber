use crate::gc::Gc;
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

    pub fn classify(&self) -> Value {
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

    pub fn new_int(x: i64) -> BlockPtr {
        unsafe { Self::new(KnownTag::Int, [mem::transmute(x)]) }
    }

    pub fn new_float(x: f64) -> BlockPtr {
        unsafe { Self::new(KnownTag::Float, [mem::transmute(x)]) }
    }

    fn new<const N: usize>(tag: KnownTag, fields: [BlockPtr; N]) -> BlockPtr {
        let len: u16 = fields.len().try_into().unwrap();
        unsafe {
            Self::new_internal(tag, len, |block| {
                copy_nonoverlapping(fields.as_ptr(), block.add(1) as *mut BlockPtr, len as usize)
            })
        }
    }

    unsafe fn new_internal(tag: KnownTag, len: u16, f: impl FnOnce(*mut Block)) -> BlockPtr {
        let n_bytes = 8 * (len + 1) as usize;
        unsafe {
            let block = Gc::get().alloc(n_bytes) as *mut Block;
            (*block).tag = tag as u16;
            (*block).len = len;
            f(block);
            BlockPtr {
                block: NonNull::new_unchecked(block),
            }
        }
    }

    unsafe fn new_string_internal(str_len: usize, f: impl FnOnce(*mut u8)) -> BlockPtr {
        let n_words: u16 = ((str_len / 8) + 1).try_into().unwrap();
        let n_bytes = 8 * (n_words as usize);
        unsafe {
            Self::new_internal(KnownTag::String, n_words, |block| {
                let fields = block.add(1) as *mut u8;
                f(fields);
                for i in str_len..(n_bytes - 1) {
                    *fields.add(i) = 0
                }
                *fields.add(n_bytes - 1) = 7 - (str_len % 8) as u8;
            })
        }
    }

    // Currently only used in tests
    #[cfg(test)]
    fn new_string(str: &str) -> BlockPtr {
        unsafe {
            Self::new_string_internal(str.len(), |fields| {
                copy_nonoverlapping(str.as_ptr(), fields, str.len())
            })
        }
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
}

#[no_mangle]
pub extern "C" fn umber_string_append(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    let (x, y) = (x.as_str(), y.as_str());
    unsafe {
        BlockPtr::new_string_internal(x.len() + y.len(), |fields| {
            copy_nonoverlapping(x.as_ptr(), fields, x.len());
            copy_nonoverlapping(y.as_ptr(), fields.add(x.len()), y.len());
        })
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

#[cfg(test)]
mod test {
    use crate::gc::umber_gc_init;

    use super::{umber_string_append, BlockPtr};

    #[test]
    fn string_creation() {
        unsafe { umber_gc_init() };
        assert_eq!(BlockPtr::new_string("hello world").as_str(), "hello world");
        assert_eq!(BlockPtr::new_string("").as_str(), "")
    }

    #[test]
    fn string_appending() {
        unsafe { umber_gc_init() };
        assert_eq!(
            umber_string_append(BlockPtr::new_string("foo"), BlockPtr::new_string("bar")).as_str(),
            "foobar"
        )
    }
}
