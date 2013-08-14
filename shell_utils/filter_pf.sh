#!/usr/bin/zsh

# "filter packed field"

# Usage: filter_pf.sh <field name> <field value>

egrep -i "$1:($2)"
