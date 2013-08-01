#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "work.h"

static int construct_triage_tags(Work *w, const char *triage_string)
{
        int i;
        AssocArrayElem *elem;
        Tag *tag;

        tag_parse_string(triage_string, &w->triage_tags);
        aa_sort_keys(&w->triage_tags);

        for (i = 0; i < work_num_triage(w); i++) {
                elem = work_triage_elem(w, i);
                tag = (Tag *)elem->val.vval;
                elem->val.dval = strtod(tag->sval, NULL);
                // TODO: Get rid of the tag val field
                //tag->val.dval = strtod(tag->sval, NULL);
        }
        return 0;
}


static int construct_estimate_tags(Work *w, const char *estimate_string)
{
        int i;
        AssocArrayElem *elem;
        Tag *tag;

        /*
         * Store the numeric tags
         */
        // TODO: Combine this with the above loop
        tag_parse_string(estimate_string, &w->estimate_tags);
        aa_sort_keys(&w->estimate_tags);

        for (i = 0; i < work_num_estimates(w); i++) {
                elem = work_estimate_elem(w, i);
                tag = (Tag *)elem->val.vval;
                elem->val.dval = work_translate_estimate(tag->sval);
        }

        /*
         * Store the raw text tags. 
         */
        // TODO: Create a function that updates the numeric estimates when the
        // raw estimates change
        tag_parse_string(estimate_string, &w->text_estimate_tags);
        aa_sort_keys(&w->text_estimate_tags);

        for (i = 0; i < work_num_estimates(w); i++) {
                elem = work_text_estimate_elem(w, i);
                tag = (Tag *)elem->val.vval;
                elem->val.sval = tag->sval;
        }


        return 0;
}

static int construct_tags(Work *w, const char *tag_string)
{
        int i;
        AssocArrayElem *elem;
        Tag *tag;

        tag_parse_string(tag_string, &w->tags);
        aa_sort_keys(&w->tags);

        for (i = 0; i < work_num_tags(w); i++) {
                elem = work_tag_elem(w, i);
                tag = (Tag *)elem->val.vval;
                elem->val.sval = tag->sval;
        }
        return 0;
}

/*
 * This initializes a Work structure. Memory for the structure must have been
 * allocated prior to calling this function.
 */
int work_init(Work *w, const char *name, const char *triage_string,
                            const char *estimate_string, const char *tag_string)
{
        /* Copy name into work structure */
        size_t name_len = strlen(name);
        if ((w->name = (char *)malloc(name_len + 1)) == NULL)
                return -1;
        strncpy(w->name, name, name_len);
        w->name[name_len] = '\0';

        /* Initialize assoc arrays */
        aa_init(&w->triage_tags, 5, aa_string_compare);
        aa_init(&w->estimate_tags, 5, aa_string_compare);
        aa_init(&w->text_estimate_tags, 5, aa_string_compare);
        aa_init(&w->tags, 5, aa_string_compare);

        /* Construct fields */
        construct_triage_tags(w, triage_string);
        construct_estimate_tags(w, estimate_string);
        construct_tags(w, tag_string);

        return 0;
}

double work_translate_estimate(const char *est_string)
{
        double scale;
        char *stop;
        double base;

        /*
         * Get the scale factor
         */
        scale = strtod(est_string, &stop);
        if (est_string == stop)
                scale = 1.0;

        /*
         * Get base
         */
        switch(*stop) {
                case 'S':
                        base = 1.0;
                        break;

                case 'M':
                        base = 2.0;
                        break;

                case 'L':
                        base = 3.0;
                        break;

                case 'Q':
                        base = 13.0;
                        break;

                default:
                        base = 0.0;
                        break;
        }

        return scale * base;
}



/*
 * The caller of this is responsible for freeing result.
 */
AssocArray *work_sum_estimates(Work *items[], int num_items)
{
        int i;
        AssocArray *result = NULL;
        AssocArray **work_estimates = NULL;
        
        /* 
         * Set up result
         */
        result = (AssocArray *)malloc(sizeof(AssocArray));
        if (result == NULL) {
                // TODO: Log something
                goto error;
        }
        aa_init(result, 5, aa_string_compare);


        /*
         * Set up work_estimates
         */
        work_estimates =
                (AssocArray **)malloc(sizeof(AssocArray *) * num_items);
        if (work_estimates == NULL) {
                // TODO: Log something
                goto error;
        }
        for (i = 0; i < num_items; i++)
                work_estimates[i] = &items[i]->estimate_tags;

        /*
         * Perform vector sum
         */
        aa_reduce(result, work_estimates, num_items, aa_vector_sum, NULL);
        return result;

error:
        free(result);
        free(work_estimates);
        return NULL;
}

// TODO: Extract common code from above
AssocArray *work_running_total(Work *items[], int num_items)
{
        int i;
        AssocArray *result = NULL;
        AssocArray **work_estimates = NULL;
        ReduceContext context;
        
        /* 
         * Set up result
         */
        result = (AssocArray *)malloc(sizeof(AssocArray));
        if (result == NULL) {
                // TODO: Log something
                goto error;
        }
        aa_init(result, 5, aa_string_compare);


        /*
         * Set up work_estimates
         */
        work_estimates =
                (AssocArray **)malloc(sizeof(AssocArray *) * num_items);
        if (work_estimates == NULL) {
                // TODO: Log something
                goto error;
        }
        for (i = 0; i < num_items; i++)
                work_estimates[i] = &items[i]->estimate_tags;

        /*
         * Perform running total
         */
        context.num_items = num_items;
        context.cur_index = 0;
        context.scale = 1.0;

        aa_reduce(result, work_estimates, num_items, aa_running_vector_sum,
                                                                    &context);
        return result;

error:
        free(result);
        free(work_estimates);
        return NULL;
}
