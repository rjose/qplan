module Filters.Schedule.Internal where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

import Filters.Utils
import Work

type Availability = [Float] -- List corresponding to num people availabile on a given day
type SkillAvailabilities = [Availability]
type SchedAvail = ([Day], SkillAvailabilities)

scheduleWork :: [String] -> Work -> SchedAvail -> (Maybe Day, SchedAvail)
scheduleWork skills work (days, skillAvails) = result
        where
                reqStaff = [(maxPerSlot, 5.0 * numWeeks * maxPerSlot) |
                            (maxPerSlot, numWeeks) <- getRequiredStaff skills work]

                result' = zipWith (\a r -> consume a [] 0 r) skillAvails reqStaff

                endDateIndexes = map fst result'
                endDate = if any isNothing endDateIndexes
                            then Nothing 
                            else fmap (days !!) (maximum endDateIndexes)

                result = (endDate, (days, map snd result'))



consume :: [Float] -> [Float] -> Int -> (Float, Float) -> (Maybe Int, [Float])
consume avail newAvail startIndex (maxPerSlot, numDays)
        | numDays > 0 && avail == [] = (Nothing, newAvail ++ avail)
        | numDays <= 0 && newAvail == [] = (Just startIndex, newAvail ++ avail)
        | numDays <= 0 && newAvail /= [] = (Just (startIndex - 1), newAvail ++ avail)
        | otherwise = consume (tail avail) (newAvail ++ [h]) (startIndex + 1) (maxPerSlot, numDays - amt)
        where
                amt = minimum [(head avail), maxPerSlot, numDays] 
                h = (head avail) - amt

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

