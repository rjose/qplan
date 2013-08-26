#!/usr/bin/env python3.3

import argparse
import sys
import re
import utils.sectionize


sections = utils.sectionize.sectionize(sys.stdin)

def standardizeTrack(track):
        if re.match("Austin.*", track):
                return "Austin"
        elif re.match("Tablet.*", track):
                return "Tablet"
        elif track == "AUS":
                return "Austin"
        elif track == "CON":
                return "Contacts"
        elif track == "FEL":
                return "Felix"
        elif track == "OPS":
                return "Ops"
        return track

def storeWorkItem(dict, track, name, array):
        if (not track in dict):
                dict[track] = {}
        dict[track][name.lower()] = array

#
# Make the work data table
#
work = {}
rank = 0
for line in sections['Work'].split('\n')[1:]:
        rank = rank + 1
        fields = line.split('\t')
        track = standardizeTrack(fields[3])
        name = fields[4]
        triage = fields[2]
        storeWorkItem(work, track, name, [rank, triage, track, name])


# Store status info
workStatus = {}
for line in sections['Status'].split('\n')[4:]:
        fields = line.split('\t')
        track = fields[7]
        name = fields[1]
        status = fields[4]
        storeWorkItem(workStatus, track, name, [track, name, status])

# The first report is to get the status
tracks = list(workStatus.keys())
tracks.sort()

def isExpected(w):
        status = w[2]
        if (re.match("de-scope", status, re.I)):
                return False
        elif (re.match("block", status, re.I)):
                return False
        return True

def isAtRisk(w):
        status = w[2]
        if (re.match("block", status, re.I)):
                return True
        return False

def isOut(w):
        status = w[2]
        if (re.match("de-scope", status, re.I)):
                return True
        return False

def workAsString(track, name, work):
        workData = None

        name_key = name.lower()
        if track in work and name_key in work[track]:
                workData = work[track][name_key]
        if workData:
                return "#%-3s %s - %s" % (workData[0], workData[1], workData[3])
        else:
                return "%-3s %s - %s" % ("***", "**", name)

# Prints quarter status
def print_quarter_status():
        for t in tracks:
                values = list(workStatus[t].values())
                print("== %s" % t)
                print("\t-- Expected for Quarter")
                for w in values:
                        if (isExpected(w)):
                                print("\t\t%s" % workAsString(w[0], w[1], work))
        
                print("\n\t-- At Risk")
                for w in values:
                        if (isAtRisk(w)):
                                print("\t\t%s" % workAsString(w[0], w[1], work))
                print("\n\t-- Planned, but out")
                for w in values:
                        if (isOut(w)):
                                print("\t\t%s" % workAsString(w[0], w[1], work))
                print()

def print_backlog():
        print("Backlog by track")

        for t in tracks:
                print("== %s" % t)

                # Get items that are out (in status spreadsheet)
                out_items = []
                all_items = []
                for w in list(workStatus[t].values()):
                        all_items.append(w[1])
                        if (isOut(w)):
                                out_items.append(w[1])

                # Get items in Q3 that aren't in status spreadsheet
                if t in work:
                        for w in list(work[t].values()):
                                if not w[3] in all_items:
                                        out_items.append(w[3])

                for work_name in out_items:
                        print("\t%s" % workAsString(t, work_name, work))

# Read commandline arguments
parser = argparse.ArgumentParser(description='Generates status and backlog reports')
parser.add_argument('-s', action='store_true', help='Print status report')
parser.add_argument('-b', action='store_true', help='Print backlog report')
args = parser.parse_args()

if args.s:
        print_quarter_status()
elif args.b:
        print_backlog()
else:
        parser.print_help()
