use core::ffi::c_void;
use core::ptr::{copy_nonoverlapping, NonNull};
use core::{panic, ptr};

use crate::block::{Block, BlockPtr, KnownTag, Value::*};

/// This file implements a simple copying collector.

const DEFAULT_HEAP_SIZE: usize = 256 * 1000;

pub struct Gc {
    heap_capacity: usize,
    next_free: usize,
    current_heap: *mut u8,
    backup_heap: *mut u8,
}

static mut GC: Gc = Gc {
    heap_capacity: 0,
    next_free: 0,
    current_heap: ptr::null_mut(),
    backup_heap: ptr::null_mut(),
};

#[no_mangle]
pub unsafe extern "C" fn umber_gc_init() {
    GC = Gc {
        heap_capacity: DEFAULT_HEAP_SIZE,
        next_free: 0,
        current_heap: libc::malloc(DEFAULT_HEAP_SIZE) as *mut u8,
        backup_heap: libc::malloc(DEFAULT_HEAP_SIZE) as *mut u8,
    }
}

// FIXME: Functions to find and set roots: ??? LLVM can help maybe?
// I guess we need to look at: globals, the stack, registers, anywhere else?
// This signature seems pretty dodgy, fix it.
// We need to properly integrate this with LLVM: use llvm gc intrinsics to identify roots
// so that LLVM can create stack maps and generate relocation instructions. As-is this
// implementation won't be safe since we are moving values but not communicating that to
// our codegen.
fn get_roots<'a>() -> &'a [BlockPtr] {
    todo!("Implement get_roots")
}

fn immediately_reachable_objects(p: Block) -> Option<impl Iterator<Item = Block>> {
    match p.classify() {
        OtherBlock(block) => Some(block.fields().iter().filter_map(|p| p.as_block())),
        Int(_) | Float(_) | String(_) | ConstantCnstr(_) => None,
    }
}

fn walk_objects<F: FnMut(Block) -> bool>(objects: impl Iterator<Item = Block>, visit: &mut F) {
    for parent_object in objects {
        if visit(parent_object) {
            if let Some(child_objects) = immediately_reachable_objects(parent_object) {
                walk_objects(child_objects, visit)
            }
        }
    }
}

impl Gc {
    pub unsafe fn get() -> &'static mut Self {
        if GC.current_heap.is_null() {
            panic!("GC was not initialized! `umber_gc_init` must be called before use")
        } else {
            &mut GC
        }
    }

    fn copy_to_backup_heap(&mut self, block: Block, next_free_in_backup_heap: &mut usize) {
        let n_bytes = (block.header().len * 8 + 1) as usize;
        let new_block = unsafe {
            let new_ptr = self.backup_heap.add(*next_free_in_backup_heap) as *mut BlockPtr;
            copy_nonoverlapping(block.as_ptr(), new_ptr, n_bytes);
            *next_free_in_backup_heap += n_bytes;
            Block::new(NonNull::new_unchecked(new_ptr))
        };
        block.forward(new_block);
    }

    /// Run a collection by performing these steps:
    /// 1. Walk all live blocks, copying them from the current heap to the backup heap
    ///    and replacing the blocks in the current heap with forwarding pointers.
    /// 2. Fix all pointers in the backup heap to point to their new locations.
    /// 3. Swap the current and backup heaps.
    fn run_collection(&mut self) {
        let mut next_free_in_backup_heap: usize = 0;
        walk_objects(
            get_roots().iter().filter_map(|p| p.as_block()),
            &mut |block| match block.header().tag.try_into() {
                Ok(KnownTag::Forward) => false,
                _ => {
                    self.copy_to_backup_heap(block, &mut next_free_in_backup_heap);
                    true
                }
            },
        );
        unsafe { ptr::swap(self.current_heap, self.backup_heap) }
        self.next_free = next_free_in_backup_heap
    }

    /// Safety: The caller must put a `Block` with a correctly formatter header at the
    /// returned memory location.
    pub unsafe fn alloc(&mut self, n_bytes: usize) -> *mut u8 {
        if self.next_free + n_bytes >= self.heap_capacity {
            self.run_collection();
        }
        if self.next_free + n_bytes >= self.heap_capacity {
            // TODO: Grow the heap (e.g. double it)? When could we shrink it again if
            // needed? Compaction? How do other GCs work?
            // Other GCs: generational tri-color mark and sweep, with compactions to
            // shrink the heap.
            panic!("Out of memory")
        }
        let result = self.current_heap.add(self.next_free);
        self.next_free += n_bytes;
        result
    }

    pub fn is_on_heap(&self, x: *const c_void) -> bool {
        let end_of_heap = unsafe { self.current_heap.add(self.heap_capacity) };
        ((x as usize) >= (self.current_heap as usize)) && ((x as usize) < (end_of_heap as usize))
    }
}

#[no_mangle]
pub unsafe extern "C" fn umber_gc_is_on_heap(x: *const c_void) -> bool {
    Gc::get().is_on_heap(x)
}

#[no_mangle]
pub unsafe extern "C" fn umber_gc_alloc(n_bytes: usize) -> *mut u8 {
    Gc::get().alloc(n_bytes)
}

#[cfg(test)]
mod test {
    use super::{umber_gc_init, DEFAULT_HEAP_SIZE};
    use crate::block::BlockPtr;
    extern crate alloc;
    use alloc::vec::Vec;
    use rand::{Rng, SeedableRng};

    #[test]
    fn allocate_and_free_memory() {
        unsafe { umber_gc_init() };
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);
        let mut blocks = Vec::new();
        let mut allocated: usize = 0;
        while allocated < DEFAULT_HEAP_SIZE - 100 {
            let block_len: u16 = if blocks.is_empty() {
                0
            } else {
                rng.gen_range(1..=10)
            };
            let new_block = unsafe {
                BlockPtr::new_with_initializer(0, block_len, |block| {
                    (*block.header_mut()).tag = 0;
                    (*block.header_mut()).len = block_len;
                    for i in 0..block_len {
                        let existing_block = blocks[rng.gen_range(0..blocks.len())];
                        block.set_field(i, existing_block);
                    }
                })
            };
            blocks.push(new_block);
            allocated += (block_len * 8 + 1) as usize;
        }
    }
}
