use crate::block::{BlockPtr, ConstantCnstr};

impl ConstantCnstr {
    pub fn new_bool(value: bool) -> Self {
        Self::new(value.into())
    }
}

impl BlockPtr {
    pub fn as_bool(self) -> bool {
        match self.as_constant_cnstr().tag() {
            0 => false,
            1 => true,
            tag => panic!("Unexpected tag for bool: {:?}", tag),
        }
    }

    pub fn new_bool(b: bool) -> BlockPtr {
        Self {
            constant_cnstr: ConstantCnstr::new_bool(b),
        }
    }
}
