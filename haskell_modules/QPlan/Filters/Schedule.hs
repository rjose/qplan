module Filters.Schedule
        (filterString)
where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

import Filters.Schedule.Internal
import Filters.Utils
import Person
import Work


filterString :: String -> String
filterString s = result
        where
                -- TODO: Read start, end, and holiday dates from stream
                holidays' = ["Nov 28, 2013", "Nov 29, 2013", "Dec 25, 2013",
                            "Dec 26, 2013", "Dec 27, 2013", "Dec 28, 2013", "Dec 29, 2013",
                            "Dec 30, 2013", "Dec 31, 2013", "Jan 1, 2014"]
                startDate = stringToDay "Oct 7, 2013"
                endDate = stringToDay "Jan 3, 2014"

                -- TODO: Read people and their vacations from stream
                oct10 = stringToDay "Oct 10, 2013"
                appsStaff = [(Person "1" "Person1" "Mobile" "Track1" "Apps" [oct10]) ]

                days = getDays startDate endDate
                holidays = map stringToDay holidays'
                workDays = map (isWorkDay holidays) days

                skills = ["Apps"]
                appsAvail = sumAvailability $ map (getAvailability workDays) appsStaff

                schedAvail = (days, [appsAvail])

                -- TODO: Read worklines in from stream
                workline1 = "3\t30\twork\tApps:M\t1.5\tTrack1\tMobile\t8\tC23"
                work = [workFromString workline1]

                estDates = schedule skills work schedAvail

                result = show estDates




-- =============================================================================
-- Test function
--
test = do
        let result = filterString ""
        putStr result

