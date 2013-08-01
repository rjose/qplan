#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "tag.h"

#define TAG_SEP ','
#define PAIR_SEP ':'


static int find_pair_sep_index(const char *tag_string, int start, int end)
{
        int i;
        int result = -1;
        for (i = start; i <= end; i++) {
                /* Separator can't be at beginning */
                if (tag_string[i] == PAIR_SEP && i == start)
                        break;
                else if (tag_string[i] == PAIR_SEP) {
                        result = i;
                        break;
                }
        }
        return result;
}

static int find_tag_sep_index(const char *tag_string, int start)
{
        int result = start;
        while (tag_string[result]) {
                if (tag_string[result] == TAG_SEP)
                        break;
                result++;
        }
        return result;
}

/*
 * Creates a tag and makes it the new head.
 */
static int create_tag(AssocArray *array, const char *str, int tag_start,
                                                int tag_end, int pair_sep_index)
{
        char *key = NULL;
        char *val = NULL;
        Tag *tag = NULL;
        size_t key_len = pair_sep_index - tag_start;
        size_t val_len = tag_end - pair_sep_index;

        if ((key = (char *)malloc(key_len+1)) == NULL)
                goto error;
        if ((val = (char *)malloc(val_len+1)) == NULL)
                goto error;
        if ((tag = (Tag *)malloc(sizeof(Tag))) == NULL)
                goto error;

        strncpy(key, str+tag_start, key_len);
        key[key_len] = '\0';

        strncpy(val, str+pair_sep_index+1, val_len);
        val[val_len] = '\0';

        /* Store element in assoc array */
        tag->key = key;
        tag->sval = val;

        AssocArrayElem elem;
        elem.key.sval = key;
        elem.val.vval = (void *)tag;
        aa_set_element(array, &elem);

        return 0;

error:
        // TODO: Log error
        free(key);
        free(val);
        free(tag);
        return -1;
}

int tag_parse_string(const char *tag_string, AssocArray *result)
{
        int num_tags = 0;
        int tag_start = 0;
        int tag_end;
        int tag_sep_index;
        int pair_sep_index;

        /*
         * Find separation between tags and then construct tags from pairs until
         * we reach the end of the string.
         */
        do {
                tag_sep_index = find_tag_sep_index(tag_string, tag_start);
                tag_end = tag_sep_index - 1;
                pair_sep_index = 
                        find_pair_sep_index(tag_string, tag_start, tag_end);

                if (pair_sep_index <= tag_start)
                        return 0;

                if (create_tag(result, tag_string, tag_start,
                                        tag_end, pair_sep_index) != 0)
                        return 0;


                num_tags++;
                tag_start = tag_sep_index + 1;

        } while (tag_string[tag_sep_index] != '\0');

        return num_tags;
}


// TODO: Think a bit more on whether to free the key or sval
int Tag_free(Tag **tags)
{
//        Tag *head = *tags;
//        Tag *cur;
//
//        while(head) {
//                cur = head;
//                head = head->next;
//
//                free(cur->key);
//                free(cur->sval);
//                free(cur);
//        }
//        *tags = NULL;
//
        return 0;
}
