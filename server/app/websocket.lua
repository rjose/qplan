local WebSocket = {}

local connections = {}

function WebSocket.register_connection(connfd)
        print("Register connection", connfd)
        connections[connfd] = 1
        return 1
end

function WebSocket.deregister_connection(connfd)
        print("Deregister connection", connfd)
        connections[connfd] = nil
        return 1
end

function WebSocket.broadcast(message)
        for k, _ in pairs(connections) do
                push_message(k, message)
        end
end

return WebSocket
