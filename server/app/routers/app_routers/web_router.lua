local Tags = require('tags')
local Work = require('work')
local Filters = require('filters')
local RequestRouter = require('request_router')
local func = require('functional')
local json = require('json')

--==============================================================================
-- Local declarations
--
local AppWebRouter = {}
local APP_RESOURCE_INDEX = 4

local handle_work_req
local gather_tracks
local group_by_skill
local format_number


--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Handles web requests for app data in an HTML format.
--
-- The routes are like "/app/web/<resource>".
--
function AppWebRouter.handle_req(req)
        if req.path_pieces[APP_RESOURCE_INDEX] == 'work' then
                return handle_work_req(req)
        end

        return RequestRouter.construct_response(400, "application/json", "")
end


--==============================================================================
-- Local functions
--

--------------------------------------------------------------------------------
-- Returns json object representing work for a track and triage level.
--
function handle_work_req(req)
        -- Get track and triage from request
        local track = 'All'
        if req.qparams['track'] then
                track = req.qparams['track'][1]
                track = string.gsub(track, "%%20", " ")
        end

        local triage = 1.5
        if req.qparams['triage'] then
                triage = req.qparams['triage'][1] + 0
        end

        -- Select work items
        local all_work_items = req.plan:get_work_items()
        local work_items = all_work_items

        -- Filter out unneeded tracks and resources
        local track_staff = req.plan.staff
        local available = func.shallow_copy(req.plan:team_bandwidth(req.plan.staff))
        if track ~= 'All' then
                local filters = {Filters.make_track_filter(track)}

                work_items = func.filter(all_work_items, filters)

                -- Look up available by track assignment
                track_staff = func.filter(req.plan.staff, filters)
                available = req.plan:team_bandwidth(track_staff)
        end

        -- Compute net supply
        local _, _, net_supply = Work.find_feasible_line(work_items, available)


        -- Create list of work items above or equal to "triage"
        local triage_filter = Filters.make_downto_triage_filter(triage)
        local triage_work_items = func.filter(work_items, triage_filter)

        -- Convert to num people
        available = req.plan:to_num_people(available)
        local demand = req.plan:to_num_people(Work.sum_demand(triage_work_items))
        local skills = func.get_table_keys(demand)

        -- Construct result
        local result = {}

        result.tracks = gather_tracks(all_work_items)

        -- Come up with number of people assigned
        local net_left = {}
        for _, skill in ipairs(skills) do
                local avail = available[skill] or 0
                net_left[skill] = avail - demand[skill]
        end

        -- Add staff list
        result.staff_by_skill = {}
        local staff_by_skill = group_by_skill(track_staff)
        for skill, people in pairs(staff_by_skill) do
                local names = {}
                for _, p in ipairs(people) do
                        names[#names+1] = p.name
                end
                table.sort(names)
                result.staff_by_skill[skill] = names
        end


        result.staffing_stats = {
                ["skills"]= skills,
                ["required"]= func.map_table(format_number, demand),
                ["available"]= func.map_table(format_number, available),
                ["net_left"]= func.map_table(format_number, net_left)
        }

        result.work_items = {}
        for i, w in ipairs(work_items) do
                local new_item = {}
                new_item.rank = w.rank
                new_item.triage = w:merged_triage()
                new_item.name = w.name
                new_item.track = w.tags.track
                new_item.estimate = Tags.tags_to_string(w.estimates, ", ")
                if w:is_any_demanded_skill_negative(net_supply[i]) then
                        new_item.feasible = false
                else
                        new_item.feasible = true
                end
                result.work_items[#result.work_items+1] = new_item
        end

        -- Return response
        return RequestRouter.construct_response(
                                        200, "application/json", json.encode(result))
end


--==============================================================================
-- Local functions
--

--------------------------------------------------------------------------------
-- Returns an array of track names for a set of work items.
--
gather_tracks = function(work_items)
        local track_hash = {}
        for _, w in ipairs(work_items) do
                track_hash[w.tags.track] = 1
        end
        return func.get_table_keys(track_hash)
end


--------------------------------------------------------------------------------
-- Groups a set of items by skill keys.
--
-- NOTE: This may be applied to either work items or people (or anything else
-- that has a skills field.
--
group_by_skill = function(items)
        -- TODO: Handle multiple skills
        local get_skill = function(item)
                for skill, frac in pairs(item.skills) do
                        -- Just return the first one way find
                        return skill
                end
        end

        return func.group_items(items, get_skill)
end


--------------------------------------------------------------------------------
-- Used to format numbers as strings.
--
format_number = function(num)
        return string.format("%.1f", num)
end

return AppWebRouter
