local Tags = require('field_tags')
local RequestRouter = require('request_router')

--==============================================================================
-- Local declarations
--
local AppTextRouter = {}
local APP_RESOURCE_INDEX = 4

--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Handles web requests for app data in a plain text format.
--
-- The routes are like "/app/text/<resource>".
--
function AppTextRouter.handle_req(req)
        -- TODO: Add stuff here to create new pipelines. This could tie into an
        -- mqplan app.

        return RequestRouter.construct_response(400, "application/text", "")
end

return AppTextRouter
