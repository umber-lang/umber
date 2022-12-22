#include <stdio.h>
#include <stdint.h>
#include "types.h"

#define constant_cnstr(n) ((Block *)((n << 1) | 1))

const Block *unit = constant_cnstr(0);

uint64_t string_len(const Block *x)
{
    uint8_t last_byte = (uint8_t)(x->fields[x->len - 1]);
    return x->len * 8 - last_byte - 1;
}

const Block *umber_print_endline(const Block *x)
{
    uint64_t n_chars = string_len(x);
    for (uint64_t i = 0; i < x->len; i++)
    {
        uint64_t word = (uint64_t)(x->fields[i]);
        for (int j = 0; j < 8 && i * 8 + j < n_chars; j++)
        {
            int shift = (7 - j) * 8;
            putchar((char)(word >> shift));
        }
    }
    putchar('\n');
    return unit;
}