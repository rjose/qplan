#include <string.h>
#include <err.h>

#import "Testing.h"

#include "../tag.h"

void test_tag_parse_string()
{
        AssocArray array;
        aa_init(&array, 5, aa_string_compare); 

        // NOTE: *Not* freeing memory here. In production code, use Tag_free
        START_SET("Parse tag string");
        pass(tag_parse_string("Prod:1,Eng:2", &array) == 2, "Check valid tag 2");
        pass(tag_parse_string("Prod:1", &array) == 1, "Check valid tag 1");
        pass(tag_parse_string("Prod:,Eng:2", &array) == 2, "Check valid tag 3");

        // Check invalid tags
        pass(tag_parse_string(",Eng:2", &array) == 0, "Check invalid tag 1");
        pass(tag_parse_string(":2", &array) == 0, "Check invalid tag 2");
        pass(tag_parse_string("Prod", &array) == 0, "Check invalid tag 3");
        pass(tag_parse_string("", &array) == 0, "Check invalid tag 4");
        END_SET("Parse tag string");
}


int main()
{
        test_tag_parse_string();
        return 0;
}

