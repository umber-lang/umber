use crate::block::{BlockPtr, Value};
use core::cmp::{Ordering, PartialEq};

impl PartialEq for BlockPtr {
    fn eq(&self, other: &Self) -> bool {
        match (self.classify(), other.classify()) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Char(x), Value::Char(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::ConstantCnstr(x), Value::ConstantCnstr(y)) => x == y,
            (Value::OtherBlock(x), Value::OtherBlock(y)) => {
                x.header().len == y.header().len
                    && core::iter::zip(x.fields(), y.fields()).all(|(x, y)| x == y)
            }
            _ => false,
        }
    }
}

impl PartialOrd for BlockPtr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.classify(), other.classify()) {
            (Value::Int(x), Value::Int(y)) => Some(x.cmp(&y)),
            (Value::Char(x), Value::Char(y)) => Some(x.cmp(&y)),
            (Value::Float(x), Value::Float(y)) => x.partial_cmp(&y),
            (Value::String(x), Value::String(y)) => Some(x.cmp(y)),
            (Value::ConstantCnstr(x), Value::ConstantCnstr(y)) => Some(x.cmp(&y)),
            (Value::OtherBlock(x), Value::OtherBlock(y)) => {
                for (x, y) in core::iter::zip(x.fields(), y.fields()) {
                    match x.partial_cmp(y) {
                        None => return None,
                        ordering @ Some(Ordering::Less | Ordering::Greater) => return ordering,
                        Some(Ordering::Equal) => continue,
                    }
                }
                Some(x.header().len.cmp(&y.header().len))
            }
            _ => None,
        }
    }
}

#[no_mangle]
pub extern "C" fn umber_eq(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_bool(x == y)
}

#[no_mangle]
pub extern "C" fn umber_neq(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_bool(x != y)
}

#[no_mangle]
pub extern "C" fn umber_lt(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_bool(x < y)
}

#[no_mangle]
pub extern "C" fn umber_lte(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_bool(x <= y)
}

#[no_mangle]
pub extern "C" fn umber_gt(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_bool(x > y)
}

#[no_mangle]
pub extern "C" fn umber_gte(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    BlockPtr::new_bool(x >= y)
}

#[no_mangle]
pub extern "C" fn umber_compare(x: BlockPtr, y: BlockPtr) -> BlockPtr {
    x.partial_cmp(&y)
        .expect("umber_compare: incomparable values")
        .into()
}
