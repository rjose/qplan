module Filters.Schedule (filterString) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale
import Person

type Availability = [Int] -- List corresponding to num people availabile on a given day

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
                qaStaff = [(Person "3" "Person3" "Mobile" "Track1" "SET" [])]

                days = getDays startDate endDate
                holidays = map stringToDay holidays'
                workDays = map (isWorkDay holidays) days

                appsAvail = sumAvailability $ map (getAvailability workDays) appsStaff

                result = show appsAvail


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

--------------------------------------------------------------------------------
-- Converts string to day.
--
--      NOTE: If the conversion should fail, we bail.
--
stringToDay :: String -> Day
stringToDay s = result
        where
                dateFormat = "%b %e, %Y"
                result = fromJust $ parseTime defaultTimeLocale dateFormat s


--------------------------------------------------------------------------------
-- Gets a list of dates from a start and end date (inclusive).
--
getDays :: Day -> Day -> [Day]
getDays start end
        | start > end = []
        | otherwise = start:getDays (addDays 1 start) end


-- =============================================================================
-- Test function
--
test = do
        let result = filterString ""
        putStr result
