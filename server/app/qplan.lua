package.path = package.path .. ";app/?.lua;../lua_modules/?.lua"

-- NOTE: This is a global object that we refer to in the qplan server
WebUI = require('app/web_ui')
WebSocket = require('app/websocket')

function qplan_init(version)
        -- Do any init stuff here
end
