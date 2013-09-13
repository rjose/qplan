StackStream = {}

function new_stream(header)
        local result = {}
        result.header = header
        result.lines = {}
        return result
end

function StackStream.unstack(lines)
        local START = "START"
        local GATHERING = "GATHERING"
        local state = START
        local result = {}
        local cur_stream = nil
        local header = nil

        for i, line in ipairs(lines) do
                if state == START then
                        header = string.match(line, "^=====(.*)")
                        if not header then
                                io.stderr:write("Couldn't find header")
                                os.exit(1)
                        else
                                cur_stream = new_stream(header)
                                state = GATHERING
                        end
                elseif state == GATHERING then
                        header = string.match(line, "^=====(.*)")
                        if header then
                                result[#result+1] = cur_stream
                                cur_stream = new_stream(header)
                        else
                                -- Remove leading tab
                                cur_stream.lines[#cur_stream.lines+1] = string.sub(line, 2)
                        end
                else
                        io.stderr:write("Should never get here")
                        os.exit(2)
                end
        end

        -- Add last stream
        if cur_stream then
                result[#result+1] = cur_stream
        end
        return result
end


return StackStream
