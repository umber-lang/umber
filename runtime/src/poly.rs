use crate::block::{BlockPtr, ConstantCnstr, Value::*};
use core::cmp::{Ordering, PartialEq};

impl PartialEq for BlockPtr {
    fn eq(&self, other: &Self) -> bool {
        match (self.classify(), other.classify()) {
            (Int(x), Int(y)) => x == y,
            (Float(x), Float(y)) => x == y,
            (String(x), String(y)) => x == y,
            (ConstantCnstr(x), ConstantCnstr(y)) => x == y,
            (OtherBlock(x), OtherBlock(y)) => {
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
            (Int(x), Int(y)) => Some(x.cmp(&y)),
            (Float(x), Float(y)) => x.partial_cmp(&y),
            (String(x), String(y)) => Some(x.cmp(y)),
            (ConstantCnstr(x), ConstantCnstr(y)) => Some(x.cmp(&y)),
            (OtherBlock(x), OtherBlock(y)) => {
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
    BlockPtr {
        constant_cnstr: ConstantCnstr::new_bool(x == y),
    }
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
