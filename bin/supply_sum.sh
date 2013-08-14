#!/usr/bin/zsh


while getopts ":mh:" option; do
        case "$option" in 
                (m)
                        weeks_left=1
                        ;;

                (h) 
                        echo "Usage: $0 [-m] quarter [year]" >&2;
                        echo "  -m: print total people for each skill instead of man-weeks" >&2;
                        exit 1;
                        ;;

        esac
done
shift $(( OPTIND - 1))

# Figure out weeks left
if [[ -z $weeks_left ]]; then
        quarter=$1
        year=$2
        weeks_left=$(weeks_left.py $quarter $year)
fi

# Sum people skills (assuming skills are in col 3)
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
