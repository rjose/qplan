local WebRouter = require('./routers/app_routers/web_router')
local TextRouter = require('./routers/app_routers/text_router')

--==============================================================================
-- Local declarations
--

local AppRouter = {}

local handle_web_request
local handle_text_request

local APP_INDEX = 2
local DEVICE_INDEX = 3


--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Handles app requests.
--
-- These may be requests for web pages or requests for raw text. Raw text is
-- used as part of unix pipelines.
--
function AppRouter.router(req)
        -- Need something like "/app/web/rbt"
        if #req.path_pieces < 4 or req.path_pieces[APP_INDEX] ~= "app" then
                return nil
        end

        -- Like "/app/web/work"
        if req.path_pieces[DEVICE_INDEX] == "web" then
                return WebRouter.handle_req(req)
        -- Like "/app/text/work"
        elseif req.path_pieces[DEVICE_INDEX] == "text" then
                return TextRouter.handle_req(req)
        else
                return nil
        end
end


--==============================================================================
-- Local functions
--

return AppRouter
