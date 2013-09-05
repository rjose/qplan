module Work(
        Work(..),
        Estimate,
        workFromString) where


import Data.List.Split
import qualified SkillAmount as SkillAmount

type Id = String
type Estimate = SkillAmount.SkillAmount
type Value = Float

data Triage
        = P1 | P1_5 | P2 | P2_5 | P3
        deriving (Show, Eq, Ord)

data Work
        = Work { id :: Id,
                 name :: String,
                 estimate :: [Estimate],
                 triage :: Triage,
                 track :: String,
                 team :: String,
                 value :: Value,
                 prereqs :: [Id]
               }
        | WorkNone
               deriving (Show)

parseTriage :: String -> Triage
parseTriage "" = P3
parseTriage s = valTriage (read s :: Float)

valTriage :: Float -> Triage
valTriage val
        | val <= 1   = P1
        | val <= 1.5 = P1_5
        | val <= 2   = P2
        | val <= 2.5 = P2_5
        | otherwise  = P3

workFromString :: String -> Work
workFromString s = Work id name estimate triage track team value prereqs
        where
                vals = splitOn "\t" s
                id = vals !! 0
                name = vals !! 1
                estimate_str = vals !! 2
                triage_str = vals !! 3
                track = vals !! 4
                team = vals !! 5
                value_str = vals !! 6
                prereqs = splitOn "," $ vals !! 7
                value = if value_str == "" then 0 else read value_str
                estimate = SkillAmount.fromVectorString estimate_str
                triage = parseTriage triage_str


-- DATA
workline = "ABC123\tAn item of work\tApps:S,Native:M,QA:3S\t1.5\tTrack1\tMobile\t8\tB21,C23"
emptyWorkline = "\t\t\t\t\t\t\t"
work = workFromString workline
