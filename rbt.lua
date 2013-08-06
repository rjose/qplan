#!/usr/bin/env lua

require('./server/modules/string_utils')
local func = require('./server/modules/functional')

local weeks_left = arg[1]

local track = ""
local header_format = "%10s|%-40s|%-7s|%-30s"

function print_heading(track)
        print(string.format("== %s", track))
        print(string.format(header_format,
                "Rank ", "Item", "Triage", "Estimate"))
        print(string.format(header_format,
                string.rep("-", 4),
                string.rep("-", 40),
                string.rep("-", 7),
                string.rep("-", 30)
                ))
end

function print_item(fields)
        print(string.format(header_format,
                "#" .. fields[1] .. " ",
                fields[3]:truncate(40),
                fields[5],
                fields[4]))
end

function print_footer(track)
        print("")
end

for line in io.lines() do
        local fields = line:split("\t")
        if (track ~= fields[7]) then
                if track ~= "" then
                        print_footer(track)
                end
                track = fields[7]
                print_heading(track)
        end
        print_item(fields)
end
