package.path = package.path .. ";../?.lua;../../../lua_modules/?.lua"

local LuaUnit = require('luaunit')

require('test_person')

LuaUnit:run()
