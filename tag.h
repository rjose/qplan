#ifndef TAG_H
#define TAG_H

#include "assoc_array.h"

typedef struct Tag_ {
        char *key;
        char *sval;

        // TODO: Get rid of this
        union {
                long lval;
                double dval;
        } val;
} Tag;

int tag_parse_string(const char *tag_string, AssocArray *result);

#endif
