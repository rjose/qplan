#!/usr/bin/gawk -f

function parse_line(line, result)
{
        split(line, skills, ",")
        for (s in skills) {
                split(skills[s], pair, ":")
                result[pair[1]] = pair[2]
        }
}

function format_supply(left)
{
        result = ""
        for (k in left)
                indices[++j] = k
        num = asorti(left, indices)
        for (i=1; i <= num; i++) {
                fragment = sprintf("%s:%.1f,", indices[i], left[indices[i]])
                result = result fragment
        }
        return substr(result, 0, length(result)-1)
}

function subtract_demand(demand, supply, net)
{
        for (k in demand)
                keys[k] = 1
        for (k in supply)
                keys[k] = 1

        for (k in keys)
                net[k] = supply[k] - demand[k]
}

BEGIN {
        FS = "\t"
        state = "START"
}


$0 == "=====stack_streams.sh ss1" {
        state = "READ SUPPLY"
        next
}


$0 == "=====stack_streams.sh ss2" {
        state = "READ DEMAND"
        next
}

state == "READ SUPPLY" {
        parse_line($0, supply)
}

state == "READ DEMAND" {
        parse_line($0, cur_demand)
        subtract_demand(cur_demand, supply, net)
        print(format_supply(net)) 
}
