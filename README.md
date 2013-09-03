qplan
=====

To start the web UI, do

   catserve -r=/home/rjose/products/qplan/server/ -p=8888

This is how you get work and staff from a qplan server:

   curl localhost:8888/app/text/work
   curl localhost:8888/app/text/staff

This is how you might generate something that could be grouped by track:

        cat all_work.txt| extract_packed.sh 6 track | group_by.lua 7 > rbt_input.txt



This shows how to use the rbt script:

        cat rbt_input.txt | filter_pf.sh track sop| filter_num.sh 5 1 1.5 |
        rbt.lua `weeks_left.py Q4`


This shows how to use two streams to generate data for net_supply:

        (stack_streams.sh 2&;
        cat all_work.txt | cut -f 4 | running_est_sum.lua >ss2;
        cat all_staff.txt | supply_sum.sh Q4 >ss1) > net_supply_input.txt

        cat net_supply_input.txt| net_left.awk


Need to show how to use gcat to pull spreadsheet data

This is used to get status data for project status and backlog reports:
cat source2.ini | gcat.py > _status_data.txt. The format of source2.ini is:

        [Work]
        w1 = <worksheet key>:<spreadsheet index>

        [Status]
        s1 = <worksheet key>:<spreadsheet index>

This is how to generate a status report:

        cat _status_data.txt | sed '/s/Soprano/Tenor/g' | qstatus.py -s

This is how to generate a backlog report:

        cat _status_data.txt | sed '/s/Soprano/Tenor/g' | qstatus.py -b

This is a complete flow:

        cat source2.ini | gcat.py | sed 's/Soprano/Tenor/g' | qstatus.py -s
