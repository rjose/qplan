package.path = package.path .. ";app/?.lua;../lua_modules/?.lua"

-- Set up some global references we'll use in the qplan server
Router = require('app/router')
WebSocket = require('app/websocket')
