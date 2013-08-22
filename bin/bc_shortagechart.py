#!/usr/bin/env python

import http.client
import sys
import json


# TODO: Split into chart data and metadata
# TODO: Read data from stdin

result1 = {
        'command': 'chart',
        'title': 'Demo Regular Shortage Chart',
        'data': {
          'type': 'shortagechart',
          'dataset': {
            'demand': [
               {'value': 20, 'label': "Tongo"},
               {'value': 35, 'label': "Bongo"}
            ],
            'shortage': [
               {'value': 5, 'label': "BE"},
               {'value': 3, 'label': "FE"},
               {'value': 1, 'label': "QA"}
             ]
          }
}}

result2 = {
        'command': 'chart',
        'title': 'Demo Regular Shortage Chart',
        'data': {
          'type': 'shortagechart',
          'dataset': {
            'demand': [
               {'value': 20, 'label': "Tongo"},
               {'value': 35, 'label': "Bongo"}
            ],
            'shortage': [
               {'value': -15, 'label': "BE"},
               {'value': -13, 'label': "FE"},
               {'value': -5, 'label': "QA"}
             ]
          }
}}

result = result1
h = http.client.HTTPConnection('localhost:8888')
h.request("POST","/broadcast", json.dumps(result),
                                {"Content-type": "application/json"})

