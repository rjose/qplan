require('string_utils')

local RequestParser = {}


--==============================================================================
-- Local declarations
--
local parse_cookies
local parse_query_params


--==============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- Parses request string into a table.
--
-- This includes headers, cookies, and query params.
--
function RequestParser.parse_request(req_str)
        local result = {}
        result.status = 0

        local pieces = req_str:split("\r\n")

        -- Parse out request line
        local request_line_parts = pieces[1]:split(" ")
        result.method = request_line_parts[1]:lower()
        result.request_target = request_line_parts[2]

        -- Parse headers
        local headers = {}
        for i = 2,#pieces do
                if pieces[i] == "" then
                        break
                end
                local header_parts = pieces[i]:split(": ")
                headers[header_parts[1]:lower()] = header_parts[2]
        end
        result.headers = headers

        -- Parse out path pieces and query params
        local qparams_pieces = result.request_target:split("?")
        result.path_pieces = qparams_pieces[1]:split("/")

        if #qparams_pieces == 2 then
                result.qparams = parse_query_params(qparams_pieces[2])
        elseif #qparams_pieces > 2 then
                -- Shouldn't have more than two parts
                result.status = -1
                return
        end

        -- Parse cookies
        result.cookies = parse_cookies(result.headers.cookie)

        return result
end



--==============================================================================
-- Local functions
--

--------------------------------------------------------------------------------
-- Converts query param portion of URL to table.
--
-- Sample input string: "/app/web/rbt?triage=1&track=sop"
--
parse_query_params = function(qstring)
        local result = {}
        local pieces = qstring:split("&")
        for i = 1,#pieces do
                local pair = pieces[i]:split("=")
                local key = pair[1]
                result[key] = result[key] or {}
                result[key][#result[key]+1] = pair[2]
        end
        return result
end


--------------------------------------------------------------------------------
-- Converts cookie string into table.
--
-- Sample input string: 'name="Borvo"; auth="123"'
--
parse_cookies = function(cstring)
        local result = {}
        if cstring == nil then
                return result
        end

        local pieces = cstring:split("; ")
        for i = 1,#pieces do
                local pair = pieces[i]:split("=")
                -- Remove the quotes
                result[pair[1]] = string.sub(pair[2], 2, -2)
        end
        return result
end


--==============================================================================
-- Module
--
return RequestParser
