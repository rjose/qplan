#!/usr/bin/zsh

# This extracts a field from a tag-like column and adds it as the last column

col=$1
key=$2


prog='
BEGIN {
        FS = "\t"
}

{
        split($col, tags, ",")
        for (t in tags) {
                split(tags[t], pair, ":")
                hash[pair[1]] = pair[2]
        }
        printf("%s\t%s\n", $0, hash[key])

}

'

gawk -v col=$col -v key=$key $prog
