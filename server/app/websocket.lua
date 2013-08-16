--==============================================================================
-- Local declarations
--
local WebSocket = {}
local connections = {}


--==============================================================================
-- Public API
--


--------------------------------------------------------------------------------
-- Registers a websocket connection for live broadcast.
--
function WebSocket.register_connection(connfd)
        print("Register connection", connfd)
        connections[connfd] = 1
        return 1
end


--------------------------------------------------------------------------------
-- Deregisters a live broadcast websocket connection.
--
function WebSocket.deregister_connection(connfd)
        print("Deregister connection", connfd)
        connections[connfd] = nil
        return 1
end


--------------------------------------------------------------------------------
-- Broadcasts message to all registered clients.
--
function WebSocket.broadcast(message)
        for k, _ in pairs(connections) do
                push_message(k, message)
        end
end

return WebSocket
