local RequestRouter = require('request_router')

--==============================================================================
-- Local declarations
--
local BroadcastRouter = {}

local BROADCAST_INDEX = 2


--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Broadcasts a message to registered websocket clients.
--
-- This is the mechanism for pushing data to client applications.
--
function BroadcastRouter.router(req)
        if req.method ~= "post" or
           req.path_pieces[BROADCAST_INDEX] ~= "broadcast" then
                return nil
        end

        -- NOTE: WebSocket is a global variable that's tied into the server
        WebSocket.broadcast(req.body)
        return RequestRouter.construct_response(200, "application/text", "")
end

return BroadcastRouter
