/// Utility functions for creating types in the standard library
use core::cmp::Ordering;

use crate::block::{BlockPtr, ConstantCnstr};

impl From<Option<BlockPtr>> for BlockPtr {
    fn from(value: Option<BlockPtr>) -> Self {
        match value {
            None => Self {
                constant_cnstr: ConstantCnstr::new(0),
            },
            Some(value) => Self::new(0, [value]),
        }
    }
}

impl From<Ordering> for BlockPtr {
    fn from(ordering: Ordering) -> Self {
        let tag = match ordering {
            Ordering::Less => 0,
            Ordering::Equal => 1,
            Ordering::Greater => 2,
        };
        Self {
            constant_cnstr: ConstantCnstr::new(tag),
        }
    }
}
