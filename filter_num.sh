#!/usr/bin/zsh

# First, we figure out what column to look at and what's the range.
col=$1
min=$2
max=$3

if [[ $max -eq '' ]]; 
        then max=$min;
fi

# Next, we'll just filter lines that lie in this range
prog='
BEGIN {
        FS = "\t"
}

$col >= min && $col <= max {
        print $0
}
'
gawk -v col=$col -v min=$min -v max=$max $prog
