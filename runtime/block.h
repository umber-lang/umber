#ifndef UMBER_BLOCK_H
#define UMBER_BLOCK_H

#include <stdint.h>

// TODO: These are duplicated with codegen.ml. Ideally we could just write them once.
#define INT_TAG (int16_t)0x8001
#define CHAR_TAG (int16_t)0x8002
#define FLOAT_TAG (int16_t)0x8003
#define STRING_TAG (int16_t)0x8004

typedef struct Block
{
    int16_t tag;
    int16_t len;
    int32_t padding;
    struct Block *fields[];
} Block;

#define block_as_int(block) ((int64_t)block->fields[0])
#define block_as_float(block) (*(double *)(block->fields))

Block *make_int_block(int64_t x);

#define constant_cnstr(n) ((Block *)(int64_t)((n << 1) | 1))
#define UNIT constant_cnstr(0)

#endif