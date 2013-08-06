qplan
=====

This is how you might generate something that could be grouped by track:

        cat all_work.txt| extract_packed.sh 6 track | group_by.lua 7 > rbt_input.txt



This shows how to use the rbt script:

        cat rbt_input.txt | filter_track.sh sop| filter_num.sh 5 1 1.5 |
        rbt.lua `weeks_left.py Q4`


This shows how to use two streams to generate data for net_supply:

        (stack_streams.sh 2&;
        cat all_work.txt | cut -f 4 | running_est_sum.lua >ss2;
        cat all_staff.txt | supply_sum.sh Q4 >ss1) > net_supply_input.txt

        cat net_supply_input.txt| net_left.awk


Need to show how to use gcat to pull spreadsheet data
