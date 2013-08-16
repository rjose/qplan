local func = require('functional')

local RequestRouter = {}

--==============================================================================
-- NOTE ON USE:
--
-- In order to use this, you need to add routers to RequestRouter.routers. For
-- instance, to serve static files, you would do
--
--      RequestRouter.routers = {static_file_router}
--
-- See tests/test_route_request.lua for examples
--

--==============================================================================
-- Parameters
--
RequestRouter.public_dir = "public"
RequestRouter.routers = {}


--==============================================================================
-- Local variables
--

local phrases = {}
phrases[200] = "OK"
phrases[400] = "Bad request"
phrases[404] = "Not found"



--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Constructs an HTTP response string.
--
function RequestRouter.construct_response(code, content_type, content)
        local tmp = {}
        tmp[#tmp+1] = string.format("HTTP/1.1 %d %s", code, phrases[code])
        tmp[#tmp+1] = string.format("Content-Length: %d", content:len())
        tmp[#tmp+1] = string.format("Content-Type: %s", content_type)
        tmp[#tmp+1] = ""
        tmp[#tmp+1] = content
        return table.concat(tmp, "\r\n")
end


--------------------------------------------------------------------------------
-- Serves files from disk.
--
function RequestRouter.static_file_router(req)
        local result
        local path = ''
        local file
        local content_type = "text/html"
        local path_pieces = req.path_pieces

        if (#req.path_pieces == 2 and req.path_pieces[2] == '') then
                path_pieces = {"", "index.html"}
        elseif req.path_pieces[2] == 'css' then
                content_type = "text/css"
        elseif req.path_pieces[2] == 'js' then
                content_type = "application/javascript"
        end

        -- Disallow ".."
        path_pieces = func.filter(path_pieces,
                                  function(piece) return piece ~= '..' end)

        path = RequestRouter.public_dir .. table.concat(path_pieces, "/")

        -- Open file
        file = io.open(path, "r")
        if file == nil then
                result = RequestRouter.construct_response(404, "text/html", "")
        else
                local content = file:read("*all")
                file:close()
                result =
                   RequestRouter.construct_response(200, content_type, content)
        end

        return result
end


--------------------------------------------------------------------------------
-- Routes requests using all specified routers.
--
function RequestRouter.route_request(req)
        local result
        for _, r in ipairs(RequestRouter.routers) do
                result = r(req)
                if result then break end
        end

        if result == nil then
                result = RequestRouter.construct_response(404, "text/html", "")
        end

        return result
end


--==============================================================================
-- Module
--

return RequestRouter
