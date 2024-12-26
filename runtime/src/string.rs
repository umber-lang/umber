use core::ffi::c_void;
use core::ptr::copy_nonoverlapping;
use core::{slice, str};

use crate::block::{Block, BlockPtr, KnownTag};
use crate::closure::Closure;

impl Block {
    fn string_len(self) -> usize {
        let last_byte =
            unsafe { *(self.as_ptr() as *const u8).add(8 * (self.header().len as usize + 1) - 1) };
        (self.header().len as usize) * 8 - (last_byte as usize) - 1
    }

    pub fn as_str<'a>(self) -> &'a str {
        self.expect_tag(KnownTag::String);
        unsafe {
            let bytes = slice::from_raw_parts(self.as_ptr().add(1) as *const u8, self.string_len());
            str::from_utf8_unchecked(bytes)
        }
    }
}

impl BlockPtr {
    pub fn as_str<'a>(self) -> &'a str {
        self.as_block().as_str()
    }

    unsafe fn new_string_with_initializer(
        str_len: usize,
        initialize: impl FnOnce(*mut u8),
    ) -> Self {
        let n_words: u16 = ((str_len / 8) + 1).try_into().unwrap();
        let n_bytes = 8 * (n_words as usize);
        Self::new_with_initializer(KnownTag::String as u16, n_words, |block| {
            let fields = block.as_ptr().add(1) as *mut u8;
            initialize(fields);
            for i in str_len..(n_bytes - 1) {
                *fields.add(i) = 0
            }
            *fields.add(n_bytes - 1) = 7 - (str_len % 8) as u8;
        })
    }

    pub fn new_string(str: &str) -> BlockPtr {
        unsafe {
            Self::new_string_with_initializer(str.len(), |fields| {
                copy_nonoverlapping(str.as_ptr(), fields, str.len())
            })
        }
    }
}

#[no_mangle]
pub extern "C" fn umber_string_make(n: BlockPtr, c: BlockPtr) -> BlockPtr {
    let n = n.as_int() as usize;
    let c = c.as_char();
    unsafe {
        BlockPtr::new_string_with_initializer(n, |fields| {
            libc::memset(fields as *mut c_void, c as i32, n);
        })
    }
}

#[no_mangle]
pub extern "C" fn umber_string_append(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    let (x, y) = (x.as_str(), y.as_str());
    unsafe {
        BlockPtr::new_string_with_initializer(x.len() + y.len(), |fields| {
            copy_nonoverlapping(x.as_ptr(), fields, x.len());
            copy_nonoverlapping(y.as_ptr(), fields.add(x.len()), y.len());
        })
    }
}

#[no_mangle]
pub unsafe extern "C" fn umber_string_fold(s: BlockPtr, init: BlockPtr, fun: Closure) -> BlockPtr {
    s.as_str()
        .chars()
        .fold(init, |acc, c| fun.apply2(acc, BlockPtr::new_char(c)))
}

#[cfg(test)]
mod test {
    use crate::{block::BlockPtr, gc::umber_gc_init};

    use super::umber_string_append;

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
