module Test.TestSchedule (testScheduleWork) where

import Test.HUnit hiding (State)

import Control.Monad.State
import Data.Maybe
import Data.Time.Calendar

import Filters.Schedule
import Filters.Schedule.Internal
import Filters.Utils
import Work






testScheduleWork :: Assertion
testScheduleWork = do
        assertEqual "" [Just date1, Just date2] result
        where
                startDate = stringToDay "Sep 29, 2013"
                endDate = stringToDay "Oct 31, 2013"
                date1 = stringToDay "Oct 4, 2013"
                date2 = stringToDay "Oct 9, 2013"

                workline1 = "ABC123\t30\tAn item of work\tApps:S\t1.5\tTrack1\tMobile\t8\tB21,C23"
                w1 = workFromString workline1
                work = [w1, w1]

                skills = ["Apps", "Native", "Web"]
                days = getDays startDate endDate
                workweek = [0, 2, 2, 1, 1, 1, 0]
                schedAvail = (days, [take (length days) $ cycle workweek])

                schedule = do
                        ds <- mapM (state . scheduleWork skills) work
                        return ds
                (result, _) = runState schedule schedAvail


testConsumeResource :: Assertion
testConsumeResource = do
        assertEqual "Consume successfully" (Just 1, [0, 0, 1]) result1
        assertEqual "Exhaust avail" (Nothing, [0, 0, 0]) result2
        assertEqual "Just exhaust avail" (Just 2, [0, 0, 0]) result3
        assertEqual "Multiple avail" (Just 2, [1, 1, 1]) result4
        assertEqual "Parallel work" (Just 1, [0, 1, 2]) result5
        assertEqual "No work" (Just 0, [1, 1, 1]) result6
                where
                        result1 = consume [1, 1, 1] [] 0 (1, 2)
                        result2 = consume [1, 1, 1] [] 0 (1, 4)
                        result3 = consume [1, 1, 1] [] 0 (1, 3)
                        result4 = consume [2, 2, 2] [] 0 (1, 3)
                        result5 = consume [2, 2, 2] [] 0 (2, 3)
                        result6 = consume [1, 1, 1] [] 0 (0, 0)

----------------------------------------------------------------------------------
---- Tests scheduling of one work item with one skill required.
----
----      Nothing is in parallel. There is only one availability list.
----
--testNoParallelOneSkill :: Assertion
--testNoParallelOneSkill = do
--        assertEqual "Result 1" [Just date1] result 
--        where
--                startDate = stringToDay "Sep 29, 2013"
--                endDate = stringToDay "Oct 5, 2013"
--                date1 = stringToDay "Oct 4, 2013"
--
--                workline = "ABC123\t30\tAn item of work\tApps:S\t1.5\tTrack1\tMobile\t8\tB21,C23"
--                w = workFromString workline
--                work = [w]
--
--                skills = ["Apps"]
--                days = getDays startDate endDate
--                schedAvail = (days, [[0, 1, 1, 1, 1, 1, 0]], [0])
--
--                schedule = do
--                        ds <- mapM (state . scheduleWork skills) work
--                        return ds
--                (result, _) = runState schedule schedAvail



--testSchedule :: Assertion
--testSchedule = do
--        testNoParallelOneSkill
