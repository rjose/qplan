#!/usr/bin/env python

import http.client
import sys
import json


# TODO: Split into chart data and metadata

result = {'command': 'chart',
          'title': 'Demo Quadchart',
          'data': {
              'type': 'quadchart',
              'options': {},
              'dataset': [
        {'name': 'Alpha', 'has_ext_prereq': True, 'effort': 5, 'value': 20},
        {'name': 'Beta', 'has_ext_prereq': True, 'effort': 450, 'value': 90},
        {'name': 'Gamma', 'is_ext_prereq': True, 'effort': 250, 'value': 50},
        {'name': 'Upsilon', 'effort': 100, 'value': 30}
           ]
}}

h = http.client.HTTPConnection('localhost:8888')
h.request("POST","/broadcast", json.dumps(result),
                                {"Content-type": "application/json"})

