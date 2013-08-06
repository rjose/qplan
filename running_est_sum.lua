#!/bin/env lua

require('./server/modules/string_utils')
local func = require('./server/modules/functional')

-- TODO: Move these to own files
function parse_tags(tag_string)
	local result = {}

	if not tag_string then return result end

	-- First split on multiple tags
	tags = tag_string:split(",")
	for _, str in pairs(tags) do
		local tag, value = unpack(str:split(":"))
		result[tag] = value
	end

	return result
end

function translate_estimate(est_string)
        local scalar = 1
        local unit
        local units = {["S"] = 1, ["M"] = 2, ["L"] = 3, ["Q"] = 13}

        -- Look for something like "4L"
        for u, _ in pairs(units) do
                scalar, unit = string.match(est_string, "^(%d*)(" .. u .. ")")
                if unit then break end
        end

        -- If couldn't find a unit, then return 0
        if unit == nil then
                -- io.stderr:write(string.format("Unable to parse: %s\n", est_string))
                return 0
        end

        -- If couldn't find a scalar, it's 1
        if scalar == "" then scalar = 1 end

        return scalar * units[unit]
end

function tags_to_string(tags, sep)
        if not tags then
                return ""
        end

	sep = sep or ","

	local keys = func.get_table_keys(tags)
	table.sort(keys)


        local result = ""
	for _, key in ipairs(keys) do
                if tags[key] and tags[key] ~= '' then
                        result = result .. string.format("%s:%s" .. sep, key, tags[key])
                end
        end

        -- Strip trailing comma
        return result:sub(1, -(1 + string.len(sep)))
end

function convert_estimates(line)
        if line == "" then return "" end

        local converted = {}
        local estimates = parse_tags(line)
        for skill, est_str in pairs(estimates) do
                converted[skill] = translate_estimate(est_str)
        end
        return converted

end

running_sum = {}
for line in io.lines() do
        if line ~= "" then
                for skill, v in pairs(convert_estimates(line)) do
                        local val = running_sum[skill]
                        val = val or 0.0
                        running_sum[skill] = val + v
                end
        end
        print(tags_to_string(running_sum))
end
