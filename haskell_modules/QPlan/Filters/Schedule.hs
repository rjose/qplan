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

                -- TODO: Read this in from stream
                oct10 = stringToDay "Oct 10, 2013"
                appsStaff = [(Person "1" "Person1" "Mobile" "Track1" "Apps" [oct10]),
                             (Person "2" "Person2" "Mobile" "Track1" "Apps" [])]

                days = getDays startDate endDate
                holidays = map stringToDay holidays'
                workDays = map (isWorkDay holidays) days

                skills = ["Apps"]
                appsAvail = sumAvailability $ map (getAvailability workDays) appsStaff
                skillsAvail = [appsAvail]

                -- TODO: Read worklines in from stream
                workline1 = "ABC123\t30\tAn item of work\tApps:M\t1.5\tTrack1\tMobile\t8\tB21,C23"
                work = [workFromString workline1]
                reqStaff = map (getRequiredStaff skills) work

                result = show reqStaff


--------------------------------------------------------------------------------
-- Sums availabilities.
--
sumAvailability :: [Availability] -> Availability
sumAvailability [] = []
sumAvailability (a:as) = foldr (zipWith (+)) a as

--------------------------------------------------------------------------------
-- Returns a person's availability given team work days.
--
getAvailability :: [(Day, Bool)] -> Person -> Availability
getAvailability workdays person = result
        where
                unavailDays = holidays person
                avail uds wd = if or [(fst wd) `elem` uds, snd wd == False]
                                 then 0
                                 else 1
                result = map (avail unavailDays) workdays

--------------------------------------------------------------------------------
-- Given a set of holidays and a day, returns if day is a workday
--
isWorkDay :: [Day] -> Day -> (Day, Bool)
isWorkDay holidays day = result
        where
                dayOfWeek = formatTime defaultTimeLocale "%a" day
                weekdays = ["Sat", "Sun"]
                result = if or [(dayOfWeek `elem` weekdays), (day `elem` holidays)]
                            then (day, False)
                            else (day, True)


-- =============================================================================
-- Test function
--
test = do
        let result = filterString ""
        putStr result

