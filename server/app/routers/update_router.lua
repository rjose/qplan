require('string_utils')
local RequestRouter = require('request_router')
local Person = require('person')
local Work = require('work')

--==============================================================================
-- Local declarations
--
local RESOURCE_INDEX = 2

local UpdateRouter = {}



--==============================================================================
-- Public API
--


--------------------------------------------------------------------------------
-- Updates data in the plan.
--
-- This is the endpoint of a unix pipeline to update plan data.
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
                end
        end

        return nil

end
return UpdateRouter
