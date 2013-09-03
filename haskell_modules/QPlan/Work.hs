module Work () where


import Data.List.Split
import SkillAmount

type Id = String

data Estimate
        = Estimate
        deriving (Show)

data Tag
         = Tag
         deriving (Show, Eq)

data Triage
        = P1 | P1_5 | P2 | P2_5 | P3
        deriving (Show, Eq, Ord)

data Value
        = Value
        deriving (Show)

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
               deriving (Show)

workLine1 = "ABC123\tAn item of work\tApps:S,Native:M,QA:3S\t1.5\tTrack1\tMobile\t8\tB21,C23"
workLineList = splitOn "\t" workLine1
est_string = workLineList !! 2
