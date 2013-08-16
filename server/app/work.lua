--[[
Work is something that needs to be done as part of a project or plan. Work is
not a specific task for one person; it's a higher level estimate of a feature
that may require multiple skills. Likewise, there is no assignment to any
person. That will be done as part of dev cycle planning.

Estimates for work items are specified using T-shirt sizing: S, M, L, Q.  These
correspond to 1w, 2w, 3w, and 13w tasks, respectively. An optional scaling
integer may precede these estimates. For instance, "4S" would mean a 4 week
effort. The presence of a scale factor sometimes implies multiple people's
effort (e.g., 3S might mean 1 week each of Android, iOS, and mobile web). There
is no space between the factor and the estimate label. Each estimate is
specified as part of a skills table. For example: {["Native"] = "L", ["Apps"] =
"2S"}.

Estimates can be converted to weeks of effort using *get_skill_demand*. The
total demand for an array of work items can be computd using *sum_demand*.  To
get running demand totals for an array of work items, use *running_demand*.

Work often needs to be categorized into different groups. This is handled
through our "tag" mechanism. For instance, to set the track for a work item w1,
we'd do 'w1.tags.track = "money"'. To set the triage group, we'd do
'w1.tags.priority = 1'.
]]--

local Object = require('object')
local func = require('functional')
local Tags = require('tags')
local Estimates = require('estimates')

--==============================================================================
-- Local declarations
--
local add_skill_demand
local subtract_skill_demand
local running_demand
local is_any_skill_negative


--------------------------------------------------------------------------------
-- Objectifies person
--
local Work = {}
Work._new = Object._new



--==============================================================================
-- Public API
--


--------------------------------------------------------------------------------
-- Constructs a work object from a table.
--
function Work.new(options)
	id = options.id or ""
        triage = options.triage or {}
	estimates = options.estimates or {}
        name = options.name or ""
	tags = options.tags or {}

	return Work:_new{id = id .. "",
                         name = name,
	                 estimates = estimates,
	                 triage = triage,
                         tags = tags}
end



--------------------------------------------------------------------------------
-- Constructs a work object from a string and a ranking.
--
function Work.construct_work(str, rank)
	local id, name, estimate_str, triage_str, tags_str =
                                                         unpack(str:split("\t"))

        local estimates = Tags.parse_tags(estimate_str)
        local triage = Tags.parse_tags(triage_str)
        local tags = Tags.parse_tags(tags_str)

	local result = Work.new{
		id = id,
		name = name,
                triage = triage,
		estimates = estimates,
                tags = tags
	}
        result.rank = rank
	return result
end



--------------------------------------------------------------------------------
-- Returns true if work item's triage matches triage.
--
function Work.triage_filter(triage_value, work_item)
        return work_item:merged_triage() == triage_value
end


--------------------------------------------------------------------------------
-- Returns true if work item's triage is no lower (priority) than triage.
--
function Work.downto_triage_filter(triage_value, work_item)
        return work_item:merged_triage() <= triage_value
end



--------------------------------------------------------------------------------
-- Returns merged triage across all triage fields.
--
-- Merge is done by taking the highest priority in the set unless the "Triage"
-- is explicitly set, in which case that value is taken.
--
function Work:merged_triage()
        if self.triage.Triage then
                return self.triage.Triage
        end

        local min = 100

        for k, val in pairs(self.triage) do
                print(k, val)
                if val < min then
                        min = val
                end
        end
        print("merged_triage", min)
        if min == 100 then min = nil end

        return min
end



--------------------------------------------------------------------------------
-- Sums the skill demand for an array of work items.
--
-- NOTE: Returns the running totals as a second result.
--
function Work.sum_demand(work_items)
	local result = running_demand(work_items)
	return result[#result], result
end



--------------------------------------------------------------------------------
-- Returns true if any required skill is negative.
--
function Work:is_any_demanded_skill_negative(supply)
	local result = false
	for skill, demand in pairs(self.estimates) do
		if demand ~= '' and supply[skill] < 0 then
			result = true
			break
		end
	end
	return result
end


--------------------------------------------------------------------------------
-- Finds first work item that is understaffed in a ranked worklist.
--
-- NOTE: This also returns the running demand and running supply.
--
function Work.find_feasible_line(work_items, supply)
        local feasible_line = #work_items
        local demand_total, running_demand = Work.sum_demand(work_items)
        local running_supply = {}
	for i = 1,#running_demand do
		running_supply[#running_supply+1] = subtract_skill_demand(
                        supply,
                        running_demand[i]
                )
	end

	for i = 1,#running_supply do
		if is_any_skill_negative(running_supply[i]) then
			feasible_line = i - 1
			break
		end
	end

	return feasible_line, running_demand, running_supply
end

--------------------------------------------------------------------------------
-- Returns work's estimates as a table of man-week values.
--
-- The work estimates is a table of strings like {["App"] = "4Q"}
--
function Work:get_skill_demand()
        local result = {}
        for skill, est_str in pairs(self.estimates) do
                result[skill] = Estimates.translate_estimate(est_str)
        end
        return result
end


--==============================================================================
-- Local functions
--


--------------------------------------------------------------------------------
-- Adds two skill demand tables together
--
add_skill_demand = function(skill_demand1, skill_demand2)
	return func.apply_keywise_2(func.add, skill_demand1, skill_demand2)
end


--------------------------------------------------------------------------------
-- Subtracts skill_demand2 from skill_demand1
--
subtract_skill_demand = function(skill_demand1, skill_demand2)
	return func.apply_keywise_2(func.subtract, skill_demand1, skill_demand2)
end

--------------------------------------------------------------------------------
-- Computes the running demand totals for an array of work items.
--
running_demand = function(work_items)

        -- "map" get_skill_demand over work_items
        local skill_demand = {}
	for i = 1,#work_items do
                skill_demand[#skill_demand+1] = work_items[i]:get_skill_demand()
	end

        -- Compute running totals
        local result = {}
        local cur_total = {}

	for i = 1,#skill_demand do
                cur_total = add_skill_demand(cur_total, skill_demand[i])
                result[#result+1] = cur_total
	end

        return result
end


--------------------------------------------------------------------------------
-- Returns true if any value in the skills table is negative.
--
is_any_skill_negative = function(skills)
	local result = false
	for skill, avail in pairs(skills) do
		if avail < 0 then
			result = true
			break
		end
	end
	return result
end

return Work
