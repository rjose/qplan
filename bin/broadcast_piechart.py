#!/usr/bin/env python

import http.client
import sys
import json

title = sys.stdin.readline()
dataset = []

# TODO: Add support for adding metadata
for line in sys.stdin.readlines():
        fields = line.split("\t")
        dataset.append({'value': fields[1], 'label': fields[0]})

result = {'command': 'chart',
          'title': title,
          'data' : {
            'type': 'piechart',
            'dataset': dataset
          }
         }

h = http.client.HTTPConnection('localhost:8888')
h.request("POST","/broadcast", json.dumps(result),
                                {"Content-type": "application/json"})

