#include <stdint.h>
#include "block.h"

Block *umber_int_add(const Block *x, const Block *y)
{
    return make_int_block(block_as_int(x) + block_as_int(y));
}

Block *umber_int_sub(const Block *x, const Block *y)
{
    return make_int_block(block_as_int(x) - block_as_int(y));
}

Block *umber_int_mul(const Block *x, const Block *y)
{
    return make_int_block(block_as_int(x) * block_as_int(y));
}