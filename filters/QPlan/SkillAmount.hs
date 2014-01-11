-----------------------------------------------------------------------------
-- |
-- Module      :  SkillAmount
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
--
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides functions for parsing skills of the form "Apps:S,Native:2M" and
-- adding and subtracting them.
--
-----------------------------------------------------------------------------

-- =============================================================================
-- Module definition
--
module SkillAmount(
        Skill,
        SkillAmount(..),
        NumStaff,
        NumWeeks,
        skill',
        numval',
        requiredStaff,
        fromVectorString,
        skillSum,
        skillDifference,
        fromString) where


-- =============================================================================
-- Module imports
--
import Data.List
import Data.List.Split


-- =============================================================================
-- Data types
--

type NumWeeks = Float
type NumStaff = Float
type Skill = String

--------------------------------------------------------------------------------
-- | Contains enough info to do arithmetic with skill amounts.
--
data SkillAmount
        = SkillSum {skill :: Skill, numval :: Float}
        | SkillNone
        deriving (Eq, Ord)

skill' :: SkillAmount -> Skill
skill' SkillNone = ""
skill' s = skill s

numval' :: SkillAmount -> Float
numval' SkillNone = 0
numval' s = numval s

instance Show SkillAmount where
        show (SkillSum skill numval) = skill ++ ":" ++ show numval
        show SkillNone = ""


-- =============================================================================
-- Public API
--


--------------------------------------------------------------------------------
-- | Returns num staff that can work on this at once and the num weeks each.
--
requiredStaff :: SkillAmount -> (NumStaff, NumWeeks)
requiredStaff (SkillSum _ effort) = result
        where
            min_chunk = 2.0 -- man-weeks
            num_people = if effort <= min_chunk
                then 1
                else fromIntegral $ floor $ effort / min_chunk
            weeks_per_person = effort / num_people
            result = (num_people, weeks_per_person)

--------------------------------------------------------------------------------
-- | Reads multiple skill amounts from a string.
--
--      E.g., "Apps:S,Native:M,QA:3S" -> [skill1, skill2, skill3]
--
fromVectorString :: String -> [SkillAmount]
fromVectorString = sort . map fromString . splitOn ","

--------------------------------------------------------------------------------
-- | Sums a list of SkillAmounts.
--
--      The resulting list will be the sums of skills by skillname. There won't
--      be any duplicate names in the result.
--
skillSum :: [SkillAmount] -> [SkillAmount]
skillSum ss = sum
        where
                groups = groupBy (\l r -> skill' l == skill' r) $ sort ss
                sum = map addSkillAmounts groups


--------------------------------------------------------------------------------
-- | Takes the difference between two lists of skills.
--
skillDifference :: [SkillAmount] -> [SkillAmount] -> [SkillAmount]
skillDifference ls rs = difference
        where
                lsum = skillSum ls
                rsum = [SkillSum (skill' s) (- numval' s) | s <- skillSum rs]
                difference = skillSum $ concat [lsum, rsum]


--------------------------------------------------------------------------------
-- | Converts a string into a skill amount.
--
--      Can parse "Native:6.5"
--
fromString :: String -> SkillAmount
fromString "" = SkillNone
fromString s = SkillSum skillname estval
        where
                (skillname, _:eststr) = break (== ':') s
                estval = read eststr

-- =============================================================================
-- Internal functions
--

--------------------------------------------------------------------------------
-- Sums skill amounts with the same skill name.
--
addSkillAmounts :: [SkillAmount] -> SkillAmount
addSkillAmounts [] = SkillNone
addSkillAmounts all@(s:ss) = SkillSum (skill' s) $
                                  foldl (\acc x -> acc + (numval' x)) 0 all


----------------------------------------------------------------------------------
---- Converts amount strings into week values.
----
----      The input is like "S" or "3M".
----
----      NOTE: The conversion values are currently hard-coded.
----
--amount :: String -> Float
--amount "S" = 1
--amount "M" = 2
--amount "L" = 3
--amount "Q" = 13
--amount s = factor * amount unit
--        where
--               len = length s
--               (factor_str, unit) = splitAt (len - 1) s
--               factor = read factor_str :: Float
