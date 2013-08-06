#!/usr/bin/zsh

quarter=$1
year=$2

weeks_left=$(weeks_left.py $quarter $year)

prog='
BEGIN {
        FS = "\t"
}

{
        # Handle multiple skills
        split($3, skills, ",")
        for (s in skills) {
                split(skills[s], pair, ":")
                supply[pair[1]] += pair[2]
        }
}

END {
        result = ""
        for (s in supply) {
                fragment = sprintf("%s:%.1f,", s, supply[s] * weeks_left)
                result = result fragment
        }
        print(substr(result, 0, length(result) - 1))
}
'

gawk -v weeks_left=$weeks_left $prog
