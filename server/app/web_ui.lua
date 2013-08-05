local Reader = require('reader')
local Writer = require('writer')
local Person = require('person')
local RequestParser = require('request_parser')
local RequestRouter = require('request_router')
local Select = require('select')
local func = require('functional')
local JsonFormat = require('json_format')
local json = require('json')

local WebUI = {}

-- STARTUP --------------------------------------------------------------------
--

-- NOTE: plan and staff are global
plan = nil
staff = nil
local work_array = nil

local RESOURCE_INDEX = 2

local APP_INDEX = 2
local DEVICE_INDEX = 3
local APP_RESOURCE_INDEX = 4


-- REQUEST HANDLING -----------------------------------------------------------
--
function handle_app_web_staff(req)
        -- "select" staff
        local staff = staff

        -- Group by skill
        local people_by_skill, skill_tags = Select.group_people_by_skill(staff)

        -- Format results
        local result_str = JsonFormat.format_people_hash(
                                       people_by_skill, skill_tags, plan, staff)

        -- Return response
        return RequestRouter.construct_response(200, "application/json", result_str)
end


-- TODO: See if I can push the detail down into well-defined steps
function handle_app_web_work(req)
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
        local all_work_items = Select.all_work(plan)
        local work_items = all_work_items

        -- Filter out unneeded tracks and resources
        local track_staff = staff
        local available = func.shallow_copy(plan.default_supply)
        if track ~= 'All' then
                local filters = {Select.make_track_filter(track)}
                work_items = Select.apply_filters(all_work_items, filters)

                -- Look up available by track assignment
                track_staff = Select.apply_filters(staff, filters)
                available = Person.sum_bandwidth(track_staff, plan.num_weeks)
        end

        -- Come up with feasible line
        local feasible_line, _, net_supply = Work.find_feasible_line(work_items, available)


        -- Create list of work items above or equal to "triage"
        local triage_filter = Select.make_downto_triage_filter(triage)
        local triage_work_items = Select.apply_filters(work_items,
                                                           {triage_filter})
        -- Convert to num people
        available = plan:to_num_people(available)
        local demand = plan:to_num_people(Work.sum_demand(triage_work_items))
        local skills = func.get_table_keys(demand)

        -- Construct result
        local result = {}

        -- TODO: These should come in a different call
        result.tracks = Select.gather_tracks(all_work_items)

        -- Come up with number of people assigned
        local net_left = {}
        for _, skill in ipairs(skills) do
                local avail = available[skill] or 0
                net_left[skill] = avail - demand[skill]
        end

        -- Add staff list
        result.staff_by_skill = {}
        local staff_by_skill = Select.group_by_skill(track_staff)
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
                ["net_left"]= func.map_table(format_number, net_left),
                ["feasible_line"]= feasible_line
        }

        -- TODO: Move to formatting function
        result.work_items = {}
        for i, w in ipairs(work_items) do
                local new_item = {}
                new_item.rank = w.rank
                new_item.triage = w:merged_triage()
                new_item.name = w.name
                new_item.track = w.tags.track
                new_item.estimate = Writer.tags_to_string(w.estimates, ", ")
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

function handle_app_web_tracks(req)
        -- Select work items
	local work = plan:get_work_items()

        -- Group items
        local track_hash, track_tags = Select.group_by_track(work)

        -- Format results
        local result_str =
            JsonFormat.format_work_by_group(track_hash, track_tags, plan, staff)

        -- Return response
        return RequestRouter.construct_response(
                                            200, "application/json", result_str)
end

-- Handles routes like "/app/web/<resource>"
function handle_app_web_request(req)
        if req.path_pieces[APP_RESOURCE_INDEX] == 'staff' then
                return handle_app_web_staff(req)
        elseif req.path_pieces[APP_RESOURCE_INDEX] == 'work' then
                return handle_app_web_work(req)
        elseif req.path_pieces[APP_RESOURCE_INDEX] == 'tracks' then
                return handle_app_web_tracks(req)
        end

        return RequestRouter.construct_response(400, "application/json", "")
end



-- REQUEST ROUTING ------------------------------------------------------------
--
function app_router(req)
        -- Need something like "/app/web/rbt"
        if #req.path_pieces < 4 or req.path_pieces[APP_INDEX] ~= "app" then
                return nil
        end

        if req.path_pieces[DEVICE_INDEX] == "web" then
                return handle_app_web_request(req)
        end

        return nil
end

-- Handles resource updating
function resource_router(req)
        local lines
        local resource

        if req.method == 'post' then
                resource = req.path_pieces[RESOURCE_INDEX]
                if resource == 'work_items' then
                        work_array = {}
                        lines = req.body:split("\n")
                        for i, l in ipairs(lines) do
                                work_array[#work_array+1] = Reader.construct_work(l)
                        end

                elseif resource == 'assignments' then
                        staff = {}
                        lines = req.body:split("\n")
                        for _, l in ipairs(lines) do
                                staff[#staff+1] = Reader.construct_person(l)
                        end
                elseif resource == 'plan' then
                        -- TODO: Move this to the appropriate location
                        local num_weeks = 13
                        plan = Plan.new{
                                id = 1,
                                name = "QPlan",
                                num_weeks = num_weeks,
                                default_supply = Person.sum_bandwidth(staff, num_weeks),
                                cutline = 1000
                        }
                        local work_items = {}
                        local work_table = {}
                        for i = 1,#work_array do
                                local w = work_array[i]
                                w.rank = i -- Set initial rank
                                work_table[w.id] = w
                                work_items[#work_items+1] = w.id .. ''
                        end
                        plan.work_items = work_items
                        plan.work_table = work_table
                        print("Updating plan")
                end
        end

end

-- Set routers
RequestRouter.routers = {app_router, resource_router, RequestRouter.static_file_router}

function WebUI.handle_request(req_string, body)
        local req = RequestParser.parse_request(req_string)
        req.body = body
        return RequestRouter.route_request(req)
end

return WebUI
