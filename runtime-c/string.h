#ifndef UMBER_STRING_H
#define UMBER_STRING_H

#include <stdint.h>
#include "block.h"

uint64_t string_len(const Block *s);
void string_iter(const Block *s, void (*f)(uint16_t));

#endif