module SkillAmount(
        SkillAmount,
        fromVectorString,
        fromString) where

import Data.List
import Data.List.Split
import qualified Data.Set as Set

type Skill = String

data SkillAmount
        = SkillAmount {skill :: Skill,  strval :: String, numval :: Float}
        | SkillSum {skill :: Skill, numval :: Float}
        | SkillNone
        deriving (Show, Eq, Ord)

fromVectorString :: String -> [SkillAmount]
fromVectorString = sort . map fromString . splitOn ","

skillSum :: [SkillAmount] -> [SkillAmount]
skillSum ss = sum
        where
                groups = groupBy (\l r -> skill l == skill r) $ sort ss
                sum = map addSkillAmounts groups

addSkillAmounts :: [SkillAmount] -> SkillAmount
addSkillAmounts [] = SkillNone
addSkillAmounts all@(s:ss) = SkillSum (skill s) $
                                  foldl (\acc x -> acc + (numval x)) 0 all 

skillDifference :: [SkillAmount] -> [SkillAmount] -> [SkillAmount]
skillDifference ls rs = difference
        where
                lsum = skillSum ls
                rsum = [SkillSum (skill s) (- numval s) | s <- skillSum rs]
                difference = skillSum $ concat [lsum, rsum]


fromString :: String -> SkillAmount
fromString s = SkillAmount skillname eststr estval
        where
                (skillname, _:eststr) = break (== ':') s
                estval = amount eststr

amount :: String -> Float
amount "S" = 1
amount "M" = 2
amount "L" = 3
amount "Q" = 13
amount s = factor * amount unit
        where
               len = length s
               (factor_str, unit) = splitAt (len - 1) s
               factor = read factor_str :: Float


-- DATA
-- TODO: Move this to a test file
est_string = "Apps:S,Native:M,QA:3S"
est_string1 = "Apps:S"
est_string2 = "Apps:2S"

v1 = fromVectorString est_string
