#ifndef ASSOC_ARRAY_H
#define ASSOC_ARRAY_H


/* ============================================================================
 * Data Structures
 */

typedef struct AssocArrayElem_ {
        union {
                char *sval;
                long lval;
                void *vval;
        } key;

        union {
                double dval;
                long lval;
                char *sval;
                void *vval;
        } val;
} AssocArrayElem;


typedef struct AssocArray_ {
        size_t capacity;
        int num_elements;
        AssocArrayElem *elements;

        int (*compare)(const void *k1, const void *k2);
        int (*destroy)(AssocArrayElem *elem);
} AssocArray;


/* This is used in the reduce calls */
typedef struct ReduceContext_ {
        int num_items;
        int cur_index;
        double scale;
} ReduceContext;


/* ============================================================================
 * Public API
 */

int aa_init(AssocArray *array, int num_elem, 
                                int (*compare)(const void *k1, const void *k2));

void aa_free(AssocArray *array);

int aa_set_element(AssocArray *array, const AssocArrayElem *elem);

AssocArrayElem *aa_get_element(const AssocArray *array, void *key);

void aa_sort_keys(AssocArray *array);

int aa_reduce(AssocArray *result, const AssocArray **assoc_arrays, size_t n,
           int (*f)(AssocArray *result, const AssocArray *other, void *context),
                                                                 void *context);

#define aa_num_elements(array) ((array)->num_elements)

#define aa_element(array, i) (&(array)->elements[i])

int aa_string_compare(const void *k1, const void *k2);

/* Adds result to other by key, storing the result in result. Assuming the
 * results are doubles. */
int aa_vector_sum(AssocArray *result, const AssocArray *other, void *context);

/* Computes running totals by key. Values are double arrays. */
int aa_running_vector_sum(AssocArray *result, const AssocArray *other, 
                                                                 void *context);

#endif

