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
        // Rust's Ordering type is the same as ours: `Less | Equal | Greater`, so we can
        // directly take the enum value as the tag.
        Self {
            constant_cnstr: ConstantCnstr::new(ordering as u64),
        }
    }
}
