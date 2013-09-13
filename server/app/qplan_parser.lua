-- TODO: Get rid of this
package.path = package.path .. ";app/?.lua;../../lua_modules/?.lua"
require('string_utils')

StackStream = require('stack_stream')

local Parser = {}

function parseDate(str)
        local months = {Jan=1, Feb=2, Mar=3, Apr=4, May=5, Jun=6, Jul=7, Aug=8, Sep=9, Oct=10, Nov=11, Dec=12}
        mon, day, year = str:match("(%a+) (%d+), (%d+)")
        result = os.time({day=day, month=months[mon], year=year})
        return result
end

function diffDays(d2, d1)
        local secPerDay = 86400
        return (d2 - d1)/secPerDay
end


function handleParamsStream(stream, out_data)
        local line = stream.lines[1]
        local fields = line:split("\t")
        out_data.start_date = parseDate(fields[1])
        out_data.end_date = parseDate(fields[2])
        out_data.num_weeks = diffDays(out_data.end_date, out_data.start_date) / 7.0
end

function stringArrayFromStream(stream)
        local result = {}
        for _, item in ipairs(stream.lines) do
                result[#result+1] = item
        end
        return result
end

-- Return manpower by track and skill.
function handleManpowerStream(stream, out_data)
        local result = {}
        for _, line in ipairs(stream.lines) do
                local fields = line:split("\t")
                for i, f in ipairs(fields) do
                        fields[i] = fields[i] + 0
                end
                result[#result+1] = fields
        end
        out_data.manpower = result
end

function handleTrackDemandStream(stream, out_data)
        local result = {}
        local triageStreams = StackStream.unstack(stream.lines)
        for _, s in ipairs(triageStreams) do
                local triageSet = {}
                for _, line in ipairs(s.lines) do
                        local fields = line:split("\t")
                        for i, f in ipairs(fields) do
                                fields[i] = f + 0
                        end
                        triageSet[#triageSet+1] = fields
                end
                result[#result+1] = triageSet
        end
        out_data.track_demand = result
end

function handleStaffStream(stream, out_data)
        local trackGroups = {}
        local trackStreams = StackStream.unstack(stream.lines)
        for _, trackStream in ipairs(trackStreams) do
                skillStreams = StackStream.unstack(trackStream.lines)
                skillGroups = {}
                for _, skillStream in ipairs(skillStreams) do
                        local people = {}
                        for _, line in ipairs(skillStream.lines) do
                                local person = {}
                                person.name = line
                                people[#people+1] = person
                        end
                        skillGroups[#skillGroups+1] = people
                end
                trackGroups[#trackGroups+1] = skillGroups
        end
        out_data.track_staff = trackGroups
end

function handleTrackWork(stream, out_data)
        local trackGroups = {}
        local trackStreams = StackStream.unstack(stream.lines)
        for _, trackStream in ipairs(trackStreams) do
                local workItems = {}
                for _, line in ipairs(trackStream.lines) do
                        local fields = line:split("\t")
                        local w = {}
                        w.track = fields[1]
                        w.rank = fields[2]
                        w.triage = fields[3]
                        w.name = fields[4]
                        w.estimate = fields[5]
                        w.feasible = fields[6] == "True"
                        w.end_date = fields[7]

                        workItems[#workItems+1] = w
                end
                trackGroups[#trackGroups+1] = workItems
        end
        out_data.track_work = trackGroups
end

function printStreamLines(stream)
        for _, l in ipairs(stream.lines) do
                print(l)
        end
end

function printWork(work)
        print(work.track, work.name, work.end_date)
end

Parser.parseLines = function(lines)
        local streams = StackStream.unstack(lines)
        local stream_data = {}
        for i, s in ipairs(streams) do
                if s.header == "qplan params v1" then
                        handleParamsStream(s, stream_data)
                elseif s.header == "qplan tracks v1" then
                        stream_data.tracks = stringArrayFromStream(s)
                elseif s.header == "qplan skills v1" then
                        stream_data.skills = stringArrayFromStream(s)
                elseif s.header == "qplan triages v1" then
                        stream_data.triages = stringArrayFromStream(s)
                elseif s.header == "qplan track manpower v1" then
                        handleManpowerStream(s, stream_data)
                elseif s.header == "qplan track-triage demand v1" then
                        handleTrackDemandStream(s, stream_data)
                elseif s.header == "qplan track-skill staff v1" then
                        handleStaffStream(s, stream_data)
                elseif s.header == "qplan track work v1" then
                        handleTrackWork(s, stream_data)
                else
                        print(string.format("TODO: Handle header: %s", s.header))
                end
        end
        return stream_data
end


return Parser
