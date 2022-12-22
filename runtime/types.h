#include <stdint.h>

typedef struct Block
{
    int16_t tag;
    int16_t len;
    int32_t padding;
    struct Block *fields[];
} Block;
