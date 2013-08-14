#!/usr/bin/env lua

package.path = package.path .. ";lua_modules/?.lua"

require('string_utils')
local func = require('functional')
local estimates = require('estimates')
local Tags = require('tags')

local weeks_left = arg[1]

local track = ""
local header_format = "%10s|%-40s|%-7s|%-30s"
local total_estimates = {}
local group_estimates = {}

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

function tally_estimates(est)
        if type(est) == "string" then return end
        for k, v in pairs(est) do
                group_estimates[k] = group_estimates[k] or 0
                group_estimates[k] = group_estimates[k] + est[k]

                total_estimates[k] = total_estimates[k] or 0
                total_estimates[k] = total_estimates[k] + est[k]
        end
end

function print_footer(est)
        if weeks_left == 0 or weeks_left == nil then
                print("")
                return
        end
        local manpower = {}
        for k, v in pairs(est) do
                manpower[k] = string.format("%4.1f", est[k]/weeks_left)
        end

        print(string.format("     %s", string.rep("-", 50)))
        print(string.format("     Required people: %s",
                Tags.tags_to_string(manpower, ", ")))
        print("")
end

for line in io.lines() do
        local fields = line:split("\t")
        if (track ~= fields[7]) then
                if track ~= "" then
                        print_footer(group_estimates)
                end
                track = fields[7]
                group_estimates = {}
                print_heading(track)
        end

        -- Print item and tally
        tally_estimates(estimates.convert_estimates(fields[4]))
        print_item(fields)
end

-- Print last footer
print_footer(group_estimates)

-- Print total footer
print(string.rep("=", 80))
print_footer(total_estimates)
