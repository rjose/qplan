#!/usr/bin/env lua

package.path = package.path .. ";lua_modules/?.lua"

require('string_utils')
local func = require('functional')

local col = arg[1] + 0

if (not col) then
        print("Usage: group_by.lua col")
        os.exit(1)
end

groupings = {}

-- Group lines by column
for line in io.lines() do
        fields = line:split("\t")
        key = fields[col]

        list = groupings[key]
        list = list or {}
        list[#list+1] = line

        groupings[key] = list
end

-- Sort keys and write out
local keys = func.get_table_keys(groupings)
table.sort(keys)

for _, k in ipairs(keys) do
        list = groupings[k]
        for _, l in ipairs(list) do
                print(l)
        end
end

