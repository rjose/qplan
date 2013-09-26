package.path = package.path .. ";app/?.lua;lua_modules/?.lua"

--==============================================================================
-- Global declarations
--
-- These are references that must be available to the qplan server. Router is
-- used to manage HTTP and websocket requests. WebSocket is used to live
-- websocket clients.
--
Router = require('app/router')
WebSocket = require('app/websocket')
inifile = require('inifile')

-- Read config params
config = inifile.parse('../config.ini')
snapshot_params = config['QPlan Snapshot Service']
local host = snapshot_params['host']
local port = snapshot_params['request_port']

-- Pull the latest data from the QPlan snapshot service
Router.set_snapshot_params(host, port)
Router.update_data()
