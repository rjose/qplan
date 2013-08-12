local WebSocket = {}

function WebSocket.register_connection(connfd)
        print("Got connection", connfd)
        return 1
end

return WebSocket
