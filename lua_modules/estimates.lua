local Tags = require('./lua_modules/tags')
local result = {}

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

function convert_estimates(line)
        if line == "" then return "" end

        local converted = {}
        local estimates = Tags.parse_tags(line)
        for skill, est_str in pairs(estimates) do
                converted[skill] = translate_estimate(est_str)
        end
        return converted

end

result.convert_estimates = convert_estimates

return result
