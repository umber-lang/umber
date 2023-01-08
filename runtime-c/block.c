#include <stdlib.h>
#include "block.h"

Block *make_int_block(int64_t x)
{
    Block *block = malloc(2 * sizeof(int64_t));
    block->tag = INT_TAG;
    block->len = 1;
    block->fields[0] = (Block *)x;
    return block;
}