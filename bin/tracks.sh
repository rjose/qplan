#!/bin/zsh

# Matches "track" in all rows, selects unique value then sorts
egrep -o 'track:[^:]*' | cut -d ':' -f 2 | uniq | sort 
