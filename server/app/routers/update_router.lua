require('string_utils')
local RequestRouter = require('request_router')
local Json = require('json')

--==============================================================================
-- Local declarations
--
local RESOURCE_INDEX = 2

local UpdateRouter = {}
local add_qplan_lookup_tables = nil


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
                -- Looking for POSTs to /qplan
                resource = req.path_pieces[RESOURCE_INDEX]
                if resource == 'qplan' then
                        req.qplan.data = Json.decode(req.body)
                        add_qplan_lookup_tables(req.qplan)
                        io.stderr:write("Finished updating qplan data")

                        return RequestRouter.construct_response(200,
                                                         "application/text", "")
                end
        end

        return nil
end

--==============================================================================
-- Local functions
--

--------------------------------------------------------------------------------
-- Adds reverse lookup for track, triage, and skill fields.
--
--      These are used to reference arrays in the returned JSON.
--
function add_qplan_lookup_tables(qplan)
        qplan.track_index = {}
        for i, v in ipairs(qplan.data.tracks) do
                qplan.track_index[v] = i
        end

        qplan.triage_index = {}
        for i, v in ipairs(qplan.data.triages) do
                qplan.triage_index[v] = i
        end

        qplan.skill_index = {}
        for i, v in ipairs(qplan.data.skills) do
                qplan.skill_index[v] = i
        end
end

return UpdateRouter
