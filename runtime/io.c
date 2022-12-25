#include <stdio.h>
#include <stdint.h>
#include "block.h"
#include "string.h"

void print_char(uint16_t c)
{
    putchar(c);
}

const Block *umber_print_endline(const Block *s)
{
    string_iter(s, print_char);
    putchar('\n');
    return UNIT;
}