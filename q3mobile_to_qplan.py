#!/usr/bin/env python3.3

import sys
import re

from utils.sectionize import sectionize

def get_estimate_string(apps_est, native_est, web_est):
        result = ""
        if apps_est:
                result = result + "Apps:" + apps_est + ","
        if native_est:
                result = result + "Native:" + native_est + ","
        if web_est:
                result = result + "Web:" + web_est + ","

        # Remove trailing comma
        if result != "":
                result = result[0:-1]
        return result

sections = sectionize(sys.stdin)

def skip_name(name):
        return re.match(".*\(start", name) or \
               re.match(".*\(end", name) or \
               re.match(".*\HOLE", name) or \
               name == ""

def condition_track(track):
    if re.match("Money", track):
        return "Money"
    elif re.match("Mobilize", track):
        return "Mobilize"
    elif re.match("Austin", track):
        return "Mobilize"
    elif re.match("Tablet", track):
        return "Tablet"
    elif re.match("Soprano", track):
        return "Tenor"
    else:
        return track

def condition_tracks(tracks):
    result = []
    for t in tracks:
            result.append(condition_track(t))
    return result

def condition_skill(skill):
    if skill == "Server":
            return "Apps"
    elif re.match("BB", skill):
            return "BB"
    else:
            return skill


# TODO: Parse out holidays from stream
print("=====qplan holidays v1")
holidays = ["Nov 28, 2013", "Nov 29, 2013", "Dec 25, 2013",
            "Dec 26, 2013", "Dec 27, 2013", "Dec 28, 2013", "Dec 29, 2013",
            "Dec 30, 2013", "Dec 31, 2013", "Jan 1, 2014"]
for d in holidays:
    print("\t%s" % d)

# TODO: Have script generate params (use commandline?)
print("=====qplan params v1")
print("\t%s\t%s\t%s" % ("Oct 7, 2013", "Jan 3, 2014", "Apps:Native:Web"))

# id, name, team, track, skill
if "raw staff" in sections:
        cur_id = 1
        num_tracks = 7
        print("=====qplan staff v1")
        lines = sections["raw staff"].split("\n")[1:]
        tracks = condition_tracks(lines[0].split("\t")[:num_tracks+1])
        cur_skill = ""

        # TODO: Pull vacation data in for each person
        vacation = ""
        for line in lines[1:]:
            if (re.match("People in", line)):
                break
            fields = line.split("\t")
            if fields[0] != "":
                cur_skill = condition_skill(fields[0])
            for i in range(1, num_tracks+1):
                name = fields[i]
                if (not skip_name(name)):
                    print("\t%d\t%s\t%s\t%s\t%s\t%s" %
                          (cur_id, name, "Mobile", tracks[i], cur_skill, vacation))
                    cur_id = cur_id + 1

# id, name, estimate, triage, track, team, value, prereqs
if "raw work" in sections:
        print("=====qplan work v1")
        cur_rank = 1
        for line in sections["raw work"].split("\n")[1:]:
                fields = line.split("\t")

                rank = str(cur_rank); cur_rank = cur_rank + 1
                id = rank
                name = fields[4]
                track = condition_track(fields[3])
                triage = fields[2]
                team = "Mobile"
                value = ""
                prereqs = ""
                native_est = fields[9]
                web_est = fields[10]
                apps_est = fields[11]
                estimate = get_estimate_string(apps_est, native_est, web_est)
                values = [id, rank, name, estimate, triage, track, team, value, prereqs]
                print("\t%s" % "\t".join(values))

