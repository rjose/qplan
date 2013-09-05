#!/usr/bin/env python3.3

import sys
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

# id, name, estimate, triage, track, team, value, prereqs
if "Raw Work" in sections:
        print("=====Work")
        cur_id = 1
        for line in sections["Raw Work"].split("\n")[1:]:
                fields = line.split("\t")

                id = str(cur_id); cur_id = cur_id + 1
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
                values = [id, name, estimate, triage, track, team, value, prereqs]
                print("\t%s" % "\t".join(values))
