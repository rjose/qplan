require('string_utils')
local RequestRouter = require('request_router')
local Person = require('person')
local Work = require('work')
local Json = require('json')

--==============================================================================
-- Local declarations
--
local RESOURCE_INDEX = 2

local UpdateRouter = {}

function add_qplan_lookup_tables(qplan)
        qplan.track_index = {}
        for i, v in ipairs(qplan.data.tracks) do
                qplan.track_index[v] = i
        end

        qplan.triage_index = {}
        for i, v in ipairs(qplan.data.triages) do
                qplan.triage_index[v] = i
        end

        qplan.track_index = {}
        for i, v in ipairs(qplan.data.tracks) do
                qplan.track_index[v] = i
        end
end


--==============================================================================
-- Public API
--


--------------------------------------------------------------------------------
-- Updates data in the plan.
--
-- This is the endpoint of a unix pipeline to update plan data.
--
-- NOTE: This is a little subtle. Updating fields of req.plan or req.qplan will
-- update fields in module objects in router.lua. All requests can access this
-- info.
--
function UpdateRouter.router(req)
        local lines
        local resource

        if req.method == 'post' then
                resource = req.path_pieces[RESOURCE_INDEX]
                if resource == 'work_items' then
                        local work_array = {}
                        lines = req.body:split("\n")
                        for i, l in ipairs(lines) do
                                work_array[#work_array+1] =
                                                      Work.construct_work(l, i)
                        end
                        req.plan.work_array = work_array
                        return RequestRouter.construct_response(200,
                                                       "application/text", "")

                elseif resource == 'assignments' then
                        local staff = {}
                        lines = req.body:split("\n")
                        for _, l in ipairs(lines) do
                                staff[#staff+1] = Person.construct_person(l)
                        end
                        req.plan.staff = staff
                        return RequestRouter.construct_response(200,
                                                       "application/text", "")
                elseif resource == 'plan' then
                        -- TODO: Read num weeks from the data
                        local num_weeks = 13
                        req.plan.num_weeks = num_weeks
                        print("Updating plan")
                        return RequestRouter.construct_response(200,
                                                       "application/text", "")
                elseif resource == 'qplan' then
                        req.qplan.data = Json.decode(req.body)
                        add_qplan_lookup_tables(req.qplan)
                        print(req.qplan.track_index["Contacts"])
                        return RequestRouter.construct_response(200,
                                                       "application/text", "")

                end
        end

        return nil

end
return UpdateRouter
