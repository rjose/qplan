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
        assertEqual "No work" [] (schedule [] schedAvail)
        assertEqual "1 item, 1 skill" [Just oct4] (schedule [w1] schedAvail)
        assertEqual "2 items, 1 skill" [Just oct4, Just oct9] (schedule ws1 schedAvail)
        assertEqual "2 skills" [Just oct11] (schedule ws2 schedAvail)
        assertEqual "2 skill, then 1" [Just oct11, Just oct9] (schedule ws3 schedAvail)
        assertEqual "5 in parallel" [Just oct10] (schedule [w3] schedAvail)
        assertEqual "Push w3 out" [Just oct11, Just oct18] (schedule [w2, w3] schedAvail)
        where
                startDate = stringToDay "Sep 29, 2013"
                endDate = stringToDay "Oct 31, 2013"
                oct4 = stringToDay "Oct 4, 2013"
                oct9 = stringToDay "Oct 9, 2013"
                oct10 = stringToDay "Oct 10, 2013"
                oct11 = stringToDay "Oct 11, 2013"
                oct18 = stringToDay "Oct 18, 2013"

                w1 = workFromString "1\t3\tW1\tApps:S\t1\tTrack1\tMobile\t8\tB21"
                w2 = workFromString "2\t3\tW1\tApps:S,Web:M\t1\tTrack1\tMobile\t8\tB21"
                w3 = workFromString "3\t3\tW1\tNative:5S,Web:S\t1\tTrack1\tMobile\t8\tB21"
                ws1 = [w1, w1]
                ws2 = [w2]
                ws3 = [w2, w1]

                skills = ["Apps", "Native", "Web"]
                days = getDays startDate endDate
                appsWorkweek = [0, 2, 2, 1, 1, 1, 0]
                nativeWorkweek = [0, 3, 3, 3, 3, 3, 0]
                webWorkweek = [0, 1, 1, 1, 1, 1, 0]
                schedAvail = (days, [
                                    take (length days) $ cycle appsWorkweek,
                                    take (length days) $ cycle nativeWorkweek,
                                    take (length days) $ cycle webWorkweek
                                    ])

                schedule work avail = result'
                        where
                                (result', _) = runState (schedule' work) avail
                                schedule' work' = do
                                        ds <- mapM (state . scheduleWork skills) work'
                                        return ds



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