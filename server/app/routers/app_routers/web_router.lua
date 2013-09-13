local json = require('json')
local RequestRouter = require('request_router')

--==============================================================================
-- Local declarations
--
local AppWebRouter = {}
local APP_RESOURCE_INDEX = 4


--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Handles web requests for app data. This is tailored by app.
--
-- The routes are like "/app/web/<resource>".
--
function AppWebRouter.handle_req(req)
        if req.path_pieces[APP_RESOURCE_INDEX] == 'qplan' then
                return handle_qplan(req)
        end

        return RequestRouter.construct_response(400, "application/json", "")
end


--==============================================================================
-- Local functions
--

--------------------------------------------------------------------------------
-- Looks up track index for track based on query params of request.
--
--      This is used to index into the track arrays that are part of the data in
--      req.qplan.data.
--
function get_track_index(req, track)
        local track = 'All'
        if req.qparams['track'] then
                track = req.qparams['track'][1]
                track = string.gsub(track, "%%20", " ")
        end

        local result = req.qplan.track_index[track]

        -- Default to "All" tracks
        if not result then
                result = 0
        end
        return result
end

--------------------------------------------------------------------------------
-- Looks up triage index for triage based on query params for request.
--
function get_triage_index(req, triage)
        local triage = "1.5"
        if req.qparams['triage'] then
                triage = req.qparams['triage'][1]
        end
        local result = req.qplan.triage_index[triage]

        -- Default to first triage
        if not result then
                result = 0
        end
        return result
end


--------------------------------------------------------------------------------
-- Returns QPlan JSON data for the specified track and triage.
--
function handle_qplan(req)
        -- TODO: Hook up num weeks
        local qplan = req.qplan
        --local track_stats = qplan.data.track_stats

        local track_index = get_track_index(req, track)
        local triage_index = get_triage_index(req, triage)

        -- Construct result
        local result = {}
        result.start_date = qplan.data.start_date
        result.end_date = qplan.data.end_date
        result.num_weeks = qplan.data.num_weeks
        result.tracks = qplan.data.tracks
        result.skills = qplan.data.skills
        result.staff_by_skill = qplan.data.track_staff[track_index]
        result.work_items = qplan.data.track_work[track_index]

        -- Add track stats
        result.track_stats = {}
        result.track_stats.manpower = qplan.data.manpower[track_index]
        result.track_stats.demand = qplan.data.track_demand[track_index][triage_index]
        local net_avail = {}
        for i, mp in ipairs(result.track_stats.manpower) do
                net_avail[i] = mp - result.track_stats.demand[i]
        end
        result.track_stats.net_avail = net_avail

        -- Return response
        return RequestRouter.construct_response(
                                        200, "application/json", json.encode(result))
end

return AppWebRouter
