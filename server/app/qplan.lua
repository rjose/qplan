package.path = package.path .. ";app/?.lua;../lua_modules/?.lua"

--==============================================================================
-- Global declarations
--
-- These are references that must be available to the qplan server. Router is
-- used to manage HTTP and websocket requests. WebSocket is used to live
-- websocket clients.
--
Router = require('app/router')
WebSocket = require('app/websocket')
