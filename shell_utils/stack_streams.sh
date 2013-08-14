#!/usr/bin/zsh

# Usage: stack_streams.sh num_outputs
#
# Example: (stack_streams.sh 2&; cat README.md >ss1; cat source.ini >ss2)


# NOTE: In order to make this re-entrant, we need to take a prefix for the fifos

num=$1

pipes=()
outputs=()

#
# Name the pipes
#
for ((i=1; i <= $num; i++)); do
        pipes[$i]="ss$i"
done


#
# Create the pipes
#
for p in $pipes; do
        if [[ ! -p $p ]]; then
                mkfifo $p
        fi
done

#
# Clean up when done
#
trap "rm -f $pipes" EXIT INT

#
# Read outputs
#
for ((i=1; i <= $num; i++)); do
        outputs[$i]=$(cat $pipes[$i])
done

#
# Write output
#

for ((i=1; i <= $num; i++)); do
        echo "=====stack_streams.sh $pipes[$i]"
        echo "$outputs[$i]"
done

