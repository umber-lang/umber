#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "block.h"
#include "string.h"

typedef enum Comparison
{
    LESS = -1,
    EQUAL = 0,
    GREATER = 1,
} Comparison;

#define compare_int(x, y) llabs(x - y)
#define compare_float(x, y) fabs(x - y)

Comparison compare_poly(const Block *x, const Block *y)
{
    switch (x->tag)
    {
    case INT_TAG:
    case CHAR_TAG:
        return compare_int(block_as_int(x), block_as_int(y));
    case FLOAT_TAG:
        return compare_float(block_as_float(x), block_as_float(y));
    case STRING_TAG:;
        // TODO: Find ways to de-dup this with string_iter, general block iteration, etc.
        uint64_t n_chars1 = string_len(x);
        uint64_t n_chars2 = string_len(y);
        uint64_t n_chars = n_chars1 >= n_chars2 ? n_chars1 : n_chars2;
        for (uint64_t i = 0; i < x->len && i < y->len; i++)
        {
            uint64_t word1 = (uint64_t)(x->fields[i]);
            uint64_t word2 = (uint64_t)(y->fields[i]);
            for (int j = 0; j < 8 && i * 8 + j < n_chars; j++)
            {
                int shift = (7 - j) * 8;
                uint16_t c1 = word1 >> shift;
                uint16_t c2 = word2 >> shift;
                Comparison comp = abs(c1 - c2);
                if (comp != 0)
                    return comp;
            }
        }
        return compare_int(n_chars1, n_chars2);
    default:
        printf("UMBER RUNTIME BUG: compare_poly is not implemented for this tag %x\n", x->tag);
        exit(1);
    }
}

Block *umber_lte(Block *x, Block *y)
{
    return constant_cnstr(compare_poly(x, y) <= 0);
}