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

def condition_tracks(tracks):
    result = []
    for t in tracks:
        if re.match("Money", t):
            result.append("Money")
        elif re.match("Mobilize", t):
            result.append("Mobilize")
        else:
            result.append(t)
    return result


# id, name, team, track, skill
if "Raw Staff" in sections:
        cur_id = 1
        num_tracks = 7
        print("=====Staff")
        lines = sections["Raw Staff"].split("\n")[1:]
        tracks = condition_tracks(lines[0].split("\t")[:num_tracks+1])
        cur_skill = ""

        for line in lines[1:]:
            if (re.match("People in", line)):
                break
            fields = line.split("\t")
            if fields[0] != "":
                cur_skill = fields[0]
            for i in range(1, num_tracks+1):
                name = fields[i]
                if (not skip_name(name)):
                    print("\t%d\t%s\t%s\t%s\t%s" % (cur_id, name, "Mobile", tracks[i], cur_skill))
                    cur_id = cur_id + 1

# id, name, estimate, triage, track, team, value, prereqs
if "Raw Work" in sections:
        print("=====Work")
        cur_rank = 1
        for line in sections["Raw Work"].split("\n")[1:]:
                fields = line.split("\t")

                rank = str(cur_rank); cur_rank = cur_rank + 1
                id = rank
                name = fields[4]
                track = fields[3]
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
