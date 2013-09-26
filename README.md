qplan
=====
This contains the main logic behind the qplan filter which takes the following
stream information:

        - qplan params v1
        - qplan holidays v1
        - qplan staff v1
        - qplan work v1

and returns the following stream information:

        - qplan params v1
        - qplan tracks v1
        - qplan skills v1
        - qplan triages v1
        - qplan track manpower v1
        - qplan track-triage demand v1
        - qplan track-skill staff v1
        - qplan track work v1

This computes staffing stats for each track and estimates schedule info for each
work item taking into account holidays, staff vacations, and work estimates.

This repo contains the qplan_server repo as a subtree. This pulls data from a
snapshot service and is updated automatically via a 0mq system. See:
https://gitli.corp.linkedin.com/rjose/q4mobile for an example of usage.

In order to use this tool, you must create a snapshot service, customizing a
data conditioning script that takes raw input from sources and formats it as
inputs to qplan as described above.
