local Work = require('work')
local func = require('functional')

--==============================================================================
-- Local declarations
--
local Filters = {}


--------------------------------------------------------------------------------
-- Returns a filter for the specified track(s).
--
function Filters.make_track_filter(t)
        local tracks = {}
        if type(t) == "table" then
                tracks = t
        else
                tracks[1] = t
        end

        local result
        result = function(work_item)
                for _, track in pairs(tracks) do
                        local search_term = track:lower():split(" ")[1]
                        if work_item.tags.track:lower():find("^" .. search_term) then
                                return true
                        end
                end
                return false
        end

        return result
end


--------------------------------------------------------------------------------
-- Returns a filter that for work items down to a triage level.
--
-- For example, downto 2.5 would select work items with triage 1, 1.5, 2, and
-- 2.5.
--
function Filters.make_downto_triage_filter(triage)
        local result = function(work_item)
                return Work.downto_triage_filter(triage, work_item)
        end
        return result
end


return Filters
