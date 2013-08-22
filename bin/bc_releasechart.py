#!/usr/bin/env python

import http.client
import sys
import json


# TODO: Split into chart data and metadata

result = {
        'command': 'chart',
        'title': 'Demo Releasechart',
        'data': {
          'type': 'releasechart',
          'dataset': {
            'releaseDates': ["Aug 30, 2013", "Oct 15, 2013", "Nov 15, 2013"],
            'features': [
              {'name': "Feature1", 'expected': "Aug 15, 2013", 'target': "Aug 30, 2013"},
              {'name': "Feature2", 'expected': "Aug 25, 2013", 'target': "Aug 30, 2013"},
              {'name': "Feature3", 'expected': "Aug 15, 2013", 'target': "Aug 30, 2013"},
              {'name': "Feature4", 'expected': "Aug 25, 2013", 'target': "Aug 30, 2013"},
              {'name': "Awesomeness", 'expected': "Sep 15, 2013", 'target': "Oct 15, 2013"}
            ]
          }
}}

h = http.client.HTTPConnection('localhost:8888')
h.request("POST","/broadcast", json.dumps(result),
                                {"Content-type": "application/json"})

