local Plan = require('plan')
local AppRouter = require('./routers/app_router')
local UpdateRouter = require('./routers/update_router')
local BroadcastRouter = require('./routers/broadcast_router')

--==============================================================================
-- Local declarations
--

local RequestParser = require('request_parser')
local RequestRouter = require('request_router')
local Router = {}

local m_plan = Plan.new{
        id = 1,
        name = "QPlan",
        num_weeks = 13,
        work_array = {},
        staff = {}
}

--------------------------------------------------------------------------------
-- Routers
--
-- NOTE: When adding new routers, register them here.
--

RequestRouter.routers = {AppRouter.router,
                         UpdateRouter.router,
                         BroadcastRouter.router,
                         RequestRouter.static_file_router}

--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Sends request through routers.
--
function Router.handle_request(req_string, body)
        local req = RequestParser.parse_request(req_string)
        req.body = body

        -- Share the Plan object with all requests
        req.plan = m_plan

        return RequestRouter.route_request(req)
end

return Router
