module Filters.Utils where

import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

import Work

dateFormat :: String
dateFormat = "%b %e, %Y"

dayToString :: Day -> String
dayToString d = formatTime defaultTimeLocale dateFormat d

--------------------------------------------------------------------------------
-- Converts string to day.
--
--      NOTE: If the conversion should fail, we bail.
--
stringToDay :: String -> Day
stringToDay s = fromJust $ parseTime defaultTimeLocale dateFormat s

--------------------------------------------------------------------------------
-- Gets a list of dates from a start and end date (inclusive).
--
getDays :: Day -> Day -> [Day]
getDays start end
        | start > end = []
        | otherwise = start:getDays (addDays 1 start) end


getRequiredStaff :: [String] -> Work -> [(NumStaff, NumWeeks)]
getRequiredStaff skills work = result
        where
                result = [reqStaff | s <- skills,
                           let estimates = estimate work
                               reqStaff' = find (\e -> s == skill' e) estimates
                               reqStaff = if isNothing reqStaff'
                                          then (0, 0)
                                          else requiredStaff $ fromJust reqStaff'
                         ]

--------------------------------------------------------------------------------
-- | Returns required manpower in terms of specified skills.
--
--      The [String] list has a list of the skills that correspond to the
--      returned list of Floats.
--
getWorkManpower :: [String] -> Work -> [Float]
getWorkManpower skills work = result
        where
                result = [manpower | s <- skills,
                           let estimates = estimate work
                               manpower' = find (\e -> s == skill' e) estimates
                               manpower = if isNothing manpower'
                                          then 0
                                          else numval' $ fromJust manpower'
                         ]

joinWith :: String -> [String] -> String
joinWith _ [] = []
joinWith _ [w] = w
joinWith c (w:ws) = w ++ c ++ (joinWith c ws)


addTab :: String -> String
addTab s = "\t" ++ s
