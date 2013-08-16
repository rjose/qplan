local func = require('functional')
local Work = require('work')
local Person = require('person')
local Object = require('object')

--==============================================================================
-- Local declarations
--
local Plan = {}
Plan._new = Object._new


--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Constructs a new plan.
--
-- Within the qplan server, there is a module-wide plan that stores an array of
-- work items and a staff list. This is the source of all plan-related
-- information.
--
function Plan.new(options)
	id = options.id or ""
	name = options.name or ""
	num_weeks = num_weeks or 13 	-- Default to a quarter
	team_id = options.team_id or ""
        work_array = options.work_array or {}
        staff = options.staff or {}

        tags = options.tags or {}

	return Plan:_new{
                id = id .. "",
                name = name,
                num_weeks = num_weeks,
                work_array = work_array,
                staff = staff,
                team_id = team_id .. "",
                tags = tags
        }
end


--------------------------------------------------------------------------------
-- Returns a filtered list of work items.
--
function Plan:get_work_items(options)
	local result = {}
        options = options or {}
	local filter = options.filter or nil

        result = func.filter(self.work_array, filter)
	return result
end

--------------------------------------------------------------------------------
-- Converts skill_totals in man-weeks into num-people
--
function Plan:to_num_people(skill_totals)
        if skill_totals == nil then
                return {}
        end

	for k, _ in pairs(skill_totals) do
		skill_totals[k] = skill_totals[k] / self.num_weeks
	end
	return skill_totals
end


--------------------------------------------------------------------------------
-- Returns bandwidth of team given the number of weeks in the plan.
--
function Plan:team_bandwidth(team)
        return Person.sum_bandwidth(team, self.num_weeks)
end

return Plan
