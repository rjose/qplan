#!/usr/bin/env python

import http.client
import sys
import json

data = sys.stdin.read()

# TODO: Add flag for setting title
result = {'title': 'Raw text', 'command': 'raw', 'data': data}

h = http.client.HTTPConnection('localhost:8888')
h.request("POST","/broadcast", json.dumps(result),
                                {"Content-type": "application/json"})

