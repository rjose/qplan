#include <string.h>
#include <err.h>

#import "Testing.h"

#include "../work.h"

#define NUM_WORK_ITEMS 10

static Work m_work[NUM_WORK_ITEMS];

static void test_create_work()
{
        AssocArrayElem *elem;
        Tag *tag;

        START_SET("Create work");
        work_init(&m_work[0], "Item 1",
                      "Prod:1,Eng:2",
                      "Native:2L,Web:M,Server:Q,BB:S",
                      "track:Track1,pm:John");

        /* Check triage tags */
        pass(strcmp("Item 1", m_work[0].name) == 0, "Name matches");
        pass(2 == work_num_triage(&m_work[0]), "Know number of triage elements");

        elem = work_triage_elem(&m_work[0], 0);
        pass(strcmp("Eng", elem->key.sval) == 0, "Know first triage element");
        pass(EQ(2, elem->val.dval), "Know first triage value");


        /* Test estimate tags */
        pass(4 == work_num_estimates(&m_work[0]), "Know number of estimate elements");
        elem = work_estimate_elem(&m_work[0], 2);
        pass(strcmp("Server", elem->key.sval) == 0, "Know third estimate element");
        pass(EQ(13, elem->val.dval), "Know third estimate value");
        
        elem = work_text_estimate_elem(&m_work[0], 2);
        pass(strcmp("Q", elem->val.sval) == 0, "Know third estimate string val");

        /* Test generic tags */
        pass(2 == work_num_tags(&m_work[0]), "Know number of tags");
        elem = work_tag_elem(&m_work[0], 1);
        pass(strcmp("track", elem->key.sval) == 0, "Know tag key");
        pass(strcmp("Track1", elem->val.sval) == 0, "Know tag value");

        END_SET("Create work");

        // TODO: Clean up memory
}

static void test_translate_estimate()
{
        START_SET("Translate work estimates");
        pass(EQ(1, work_translate_estimate("S")), "Translate S");
        pass(EQ(2, work_translate_estimate("M")), "Translate M");
        pass(EQ(3, work_translate_estimate("L")), "Translate L");
        pass(EQ(13, work_translate_estimate("Q")), "Translate Q");
        pass(EQ(9, work_translate_estimate("3L")), "Translate 3L");
        pass(EQ(0, work_translate_estimate("")), "Translate ''");
        END_SET("Translate work estimates");
}

static void test_sum_estimates()
{
        AssocArray *sum;
        AssocArrayElem *elem;
        Work *work_items[2];

        work_init(&m_work[1], "Item 1",
                      "",
                      "Native:2L,Apps:Q",
                      "");
        work_init(&m_work[2], "Item 2",
                      "",
                      "Native:5L,Web:2M,Apps:S",
                      "");

        work_items[0] = &m_work[1];
        work_items[1] = &m_work[2];

        START_SET("Sum estimates");
        sum = work_sum_estimates(work_items, 2);
        pass(3 == aa_num_elements(sum), "Should be 3 skills");
        elem = aa_get_element(sum, "Native");
        pass(EQ(21.0, elem->val.dval), "Native should be 21.0");

        elem = aa_get_element(sum, "Apps");
        pass(EQ(14.0, elem->val.dval), "Apps should be 14.0");

        elem = aa_get_element(sum, "Web");
        pass(EQ(4.0, elem->val.dval), "Web should be 4.0");

        // TODO: Free "sum"
        // TODO: Free work

        END_SET("Sum estimates");
}

static void test_running_total()
{
        AssocArray *running_totals;
        AssocArrayElem *elem;
        Work *work_items[2];
        double *double_values;

        work_init(&m_work[3], "Item 3",
                      "",
                      "Native:2L,Apps:Q",
                      "");
        work_init(&m_work[4], "Item 4",
                      "",
                      "Native:5L,Web:2M,Apps:S",
                      "");

        work_items[0] = &m_work[3];
        work_items[1] = &m_work[4];

        START_SET("Running total");
        running_totals = work_running_total(work_items, 2);
        pass(3 == aa_num_elements(running_totals), "Should be 3 skills");

        /* Check Native totals */
        elem = aa_get_element(running_totals, "Native");
        double_values = (double *)elem->val.vval;
        pass(EQ(6, double_values[0]), "Native running total 1");
        pass(EQ(21, double_values[1]), "Native running total 2");


        // TODO: Free "sum"
        // TODO: Free work

        END_SET("Running total");
}

int main()
{
        test_create_work();
        test_translate_estimate();
        test_sum_estimates();
        test_running_total();
        
        return 0;
}
