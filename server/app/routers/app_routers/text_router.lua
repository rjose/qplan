local Tags = require('tags')
local RequestRouter = require('request_router')

--==============================================================================
-- Local declarations
--
local AppTextRouter = {}
local APP_RESOURCE_INDEX = 4

local handle_work_req
local handle_staff_req


--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Handles web requests for app data in a plain text format.
--
-- The routes are like "/app/text/<resource>".
--
function AppTextRouter.handle_req(req)
        if req.path_pieces[APP_RESOURCE_INDEX] == 'work' then
                return handle_work_req(req)
        elseif req.path_pieces[APP_RESOURCE_INDEX] == 'staff' then
                return handle_staff_req(req)
        end

        return RequestRouter.construct_response(400, "application/text", "")
end


--==============================================================================
-- Local functions
--

--------------------------------------------------------------------------------
-- Returns tab separated table of work items.
--
handle_work_req = function(req)
        local work = {}

        for rank, w in ipairs(req.plan.work_array) do
                work[#work+1] = string.format("%d\t%s\t%s\t%s\t%s\t%s",
                        rank, w.id, w.name,
                        Tags.tags_to_string(w.estimates, ","),
                        w:merged_triage(),
                        Tags.tags_to_string(w.tags, ","))
        end

        local result_str = table.concat(work, "\n")

        -- Return response
        return RequestRouter.construct_response(200, "application/text", result_str)
end


--------------------------------------------------------------------------------
-- Returns tab separated table of staff personnel.
--
handle_staff_req = function(req)
        local rows = {}

        for _, person in ipairs(req.plan.staff) do
                rows[#rows+1] = string.format("%s\t%s\t%s\t%s",
                        person.id, person.name,
                        Tags.tags_to_string(person.skills, ","),
                        Tags.tags_to_string(person.tags, ","))
        end

        local result_str = table.concat(rows, "\n")

        -- Return response
        return RequestRouter.construct_response(200, "application/text", result_str)
end

return AppTextRouter
