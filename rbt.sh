#/usr/bin/zsh

data=$(cat data.txt )
conditioned=$(cat data.txt | qplan_condition.awk)
cond2=$(echo -e "$data" | qplan_condition.awk)

echo -e "$cond2"
