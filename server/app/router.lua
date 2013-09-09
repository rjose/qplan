local AppRouter = require('./routers/app_router')
local UpdateRouter = require('./routers/update_router')
local BroadcastRouter = require('./routers/broadcast_router')

--==============================================================================
-- Local declarations
--

local RequestParser = require('request_parser')
local RequestRouter = require('request_router')
local Router = {}

local m_qplan = {}
m_qplan.data = nil

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

        -- Share qplan data with all requests
        req.qplan = m_qplan

        return RequestRouter.route_request(req)
end

return Router
