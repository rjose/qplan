--[[
A Person has skills that can be applied to do work. Each person has a certain
amount of bandwidth for work. The bandwidth for a team is the sum of the
bandwidth for individuals. 

A Person can have multiple skills. Their default skill distribution is specified
in the skills table. This distribution may be overridden as part of a Plan.
]]--


--==============================================================================
-- Local declarations
--

local Tags = require('tags')
local Object = require('object')


--==============================================================================
-- Objects and construction
--

--------------------------------------------------------------------------------
-- Objectifies person
--
local Person = {}
Person._new = Object._new


--------------------------------------------------------------------------------
-- Constructs a person from a table.
--
function Person.new(options)
	id = options.id or ""
        name = options.name or ""
        skills = options.skills or {}
        tags = options.tags or {}

	return Person:_new{
		id = id .. "", name = name, skills = skills, tags = tags
	}
end


--------------------------------------------------------------------------------
-- Constructs a person from string.
--
-- The string must be in this form:
--
--      "id\tname\tskills\ttags"
--
function Person.construct_person(str)
	local id, name, skills_str, tags_str = unpack(str:split("\t"))

        local skills = Tags.parse_tags(skills_str)
        local tags = Tags.parse_tags(tags_str)

	local result = Person.new{
		id = id,
		name = name,
		skills = skills,
                tags = tags
	}
	return result
end


--------------------------------------------------------------------------------
-- Returns table of a person's bandwidth by skill.
--
function Person:get_bandwidth(num_weeks)
	local result = {}
	for skill, frac in pairs(self.skills) do
		result[skill] = frac * num_weeks
	end
	return result
end


--------------------------------------------------------------------------------
-- Adds two bandwidths.
--
function add_bandwidth(b1, b2)
	local result = {}
	for k, v in pairs(b1) do result[k] = v end

        for skill, avail in pairs(b2) do
                if result[skill] then
			result[skill] = result[skill] + avail
                else
                        result[skill] = avail
                end
        end
        return result
end


--------------------------------------------------------------------------------
-- Sums skill bandwidths for array of people.
--
function Person.sum_bandwidth(people, num_weeks)
	local result = {}
	for i = 1,#people do
		result = add_bandwidth(result, people[i]:get_bandwidth(num_weeks))
	end
	return result
end


return Person
