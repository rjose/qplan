#ifndef WORK_H
#define WORK_H

#include "assoc_array.h"
#include "tag.h"

typedef struct Work_ {
        char *name;
        AssocArray triage_tags;
        AssocArray text_estimate_tags;
        AssocArray estimate_tags;
        AssocArray tags;
} Work;


int work_init(Work *w, const char *name, const char *triage_string,
                            const char *estimate_string, const char *tag_string);

double work_translate_estimate(const char *est_string);

AssocArray *work_sum_estimates(Work *items[], int num_items);

AssocArray *work_running_total(Work *items[], int num_items);

#define work_num_triage(work) ((work)->triage_tags.num_elements)

#define work_triage_elem(work, i) (&(work)->triage_tags.elements[i])

#define work_num_estimates(work) ((work)->estimate_tags.num_elements)

#define work_estimate_elem(work, i) (&(work)->estimate_tags.elements[i])

#define work_text_estimate_elem(work, i) \
        (&(work)->text_estimate_tags.elements[i])

#define work_num_tags(work) ((work)->tags.num_elements)

#define work_tag_elem(work, i) (&(work)->tags.elements[i])

#endif
