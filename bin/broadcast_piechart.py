#!/usr/bin/env python

import http.client
import sys
import json

title = sys.stdin.readline()
labels = []
values = []

# TODO: Add support for adding metadata
for line in sys.stdin.readlines():
        fields = line.split("\t")
        labels.append(fields[0])
        values.append(fields[1])

result = {'command': 'piechart',
                'data': {'title': title, 'labels': labels, 'values': values}}

h = http.client.HTTPConnection('localhost:8888')
h.request("POST","/broadcast", json.dumps(result),
                                {"Content-type": "application/json"})

