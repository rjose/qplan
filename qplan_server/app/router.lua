local Router = {}

--==============================================================================
-- Local declarations
--

local RequestParser = require('request_parser')
local RequestRouter = require('request_router')
local QPlanParser = require('qplan_parser')
local StackStream = require('stack_stream')
local json = require('json')
local zmq = require('zmq')

local m_qplan = {}
local m_snapshot_host = nil
local m_snapshot_port = nil
local context = zmq.init()

--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Sets params used to establish socket to snapshot service.
--
function Router.set_snapshot_params(host, port)
        m_snapshot_host = host
        m_snapshot_port = port
end

--------------------------------------------------------------------------------
-- Updates qplan data that the router serves.
--
--      This gets the latest data from the qplan snapshot service.
--
function Router.update_data()
        local socket = context:socket(zmq.REQ)
        socket:connect(string.format("tcp://%s:%d", m_snapshot_host, m_snapshot_port))
        socket:send("=====GET qplan app")
        local data, err = socket:recv()
        local streams = StackStream.unstack(data:split("\n"))
        
        local keyed_streams = {}
        for k, v in pairs(streams) do
                keyed_streams[v.header] = v.lines
        end
        
        -- Parse data and store SHA of data version
        m_qplan = QPlanParser.parseLines(keyed_streams["data"])
        m_qplan.version = keyed_streams["qplan app"][1]

        io.stderr:write("Finished updating qplan data\n")
        socket:close()
end


--==============================================================================
-- Internals
--

--------------------------------------------------------------------------------
-- Handles web requests for app data.
--
function handle_app_req(req)
        if not string.match(req.path, "/app/") then
                return nil
        elseif req.path == "/app/web/qplan" then
                data = get_qplan_data(req)
                return RequestRouter.construct_response(
                                   200, "application/json", json.encode(data))
        else
                return RequestRouter.construct_response(
                                   400, "application/json", "")
        end
end

--------------------------------------------------------------------------------
-- Returns QPlan JSON data for the specified track and triage.
--
function get_qplan_data(req)
        local qplan = req.qplan

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

        return result
end

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
-- Pulls the latest data when pinged.
--
--      The intent of this route is to have an external service ping us when the
--      qplan app data changes.
--
function handle_update_ping(req)
        if req.method == 'post' and req.path == "/update" then
                Router.update_data()
                return RequestRouter.construct_response(
                                        200, "application/text", "OK")
        else
                return nil
        end
end


--------------------------------------------------------------------------------
-- Sends request through routers.
--
function Router.handle_request(req_string, body)
        local req = RequestParser.parse_request(req_string)
        req.body = body

        -- Share qplan data with all requests
        req.qplan = m_qplan

        return RequestRouter.route_request(req)
end

--------------------------------------------------------------------------------
-- Request handlers
--
RequestRouter.handlers = {handle_app_req,
                          handle_update_ping,
                          RequestRouter.handle_static_file}


--------------------------------------------------------------------------------
-- Return the public module
--
return Router
