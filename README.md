qplan
=====

Starting a QPlan server
-----------------------

To get spreadsheet data:

        . cat mobile_q3.ini | gcat.py > _q3_mobile.txt

The mobile_q3.ini file should have a format like this:

        [raw work]
        w1 = <worksheet key>:<spreadsheet index>

        [raw staff]
        s1 = <worksheet key>:<spreadsheet index>

To condition the spreadsheet data into a form that the qplan filter can
consume, you'll need to write your own script to convert it to known stream
types. In this case, we'll need a
link:https://github.com/rjose/stream-specs/blob/master/work.txt["qplan work
v1"] stream and a
link:https://github.com/rjose/stream-specs/blob/master/staff.txt["qplan staff
v1"] stream. Once you have this script, you can do this:

        . cat _q3_mobile.txt | q3mobile_to_qplan.py > _qplan.txt

To get JSON data applicable for the qplan app, pipe the conditioned streams
into the qplan filter:

        . cat _qplan.txt | qplan > q3.json

To start the web UI, do

        . catserve -r=./server/ -p=8888
        . cat q3.json | sed s/\'//g | curl -X POST -d @- localhost:8888/qplan


Generating a status report
--------------------------
NOTE: This will change to use the qplan filter
This is how to generate a status report:

        cat _status_data.txt | sed '/s/Soprano/Tenor/g' | qstatus.py -s

This is how to generate a backlog report:

        cat _status_data.txt | sed '/s/Soprano/Tenor/g' | qstatus.py -b

This is a complete flow:

        cat source2.ini | gcat.py | sed 's/Soprano/Tenor/g' | qstatus.py -s
