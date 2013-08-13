#!/bin/env lua

package.path = package.path .. ";lua_modules/?.lua"

require('string_utils')
local Estimates = require('estimates')
local Tags = require('tags')

running_sum = {}
for line in io.lines() do
        if line ~= "" then
                for skill, v in pairs(Estimates.convert_estimates(line)) do
                        local val = running_sum[skill]
                        val = val or 0.0
                        running_sum[skill] = val + v
                end
        end
        print(Tags.tags_to_string(running_sum))
end
