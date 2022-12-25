#include <stdint.h>
#include "block.h"
#include "string.h"

uint64_t string_len(const Block *s)
{
    uint8_t last_byte = (uint8_t)(uint64_t)(s->fields[s->len - 1]);
    return s->len * 8 - last_byte - 1;
}

void string_iter(const Block *s, void (*f)(uint16_t))
{
    uint64_t n_chars = string_len(s);
    for (uint64_t i = 0; i < s->len; i++)
    {
        uint64_t word = (uint64_t)(s->fields[i]);
        for (int j = 0; j < 8 && i * 8 + j < n_chars; j++)
        {
            int shift = (7 - j) * 8;
            f((uint16_t)(word >> shift));
        }
    }
}
