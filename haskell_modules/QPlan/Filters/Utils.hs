module Filters.Utils
        (getWorkManpower,
         getRequiredStaff)
where

import Data.List
import Data.Maybe

import Work


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
