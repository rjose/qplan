package.path = package.path .. ";../?.lua"

local LuaUnit = require('luaunit')

--require('test_parse_request')
--require('test_route_request')
require('test_stack_stream')

LuaUnit:run()
