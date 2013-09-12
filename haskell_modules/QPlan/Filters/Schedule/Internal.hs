module Filters.Schedule.Internal where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale
import Control.Monad.State

import Filters.Utils
import Person
import Work

type Availability = [Float] -- List corresponding to num people availabile on a given day
type SkillAvailabilities = [Availability]
type SchedAvail = ([Day], SkillAvailabilities)


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
                unavailDays = vacation person
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


schedule :: [String] -> [Work] -> SchedAvail -> [Maybe Day]
schedule skills work schedAvail = result
        where
                (result, _) = runState (sched work) schedAvail
                sched ws = do
                        ds <- mapM (state . scheduleWork skills) ws
                        return ds

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

