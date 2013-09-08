module Person (
        Person(..),
        personFromString) where

import Data.List.Split

-- id, name, team, track, skill

type Id = String

data Person = Person { id :: Id,
                       name :: String,
                       team :: String,
                       track :: String,
                       skill :: String
                     }
                     deriving (Show, Eq)

instance Ord Person where
        compare l r = compare (name l) (name r)

personFromString :: String -> Person
personFromString s = Person id name team track skill
        where
                vals = splitOn "\t" s
                id = vals !! 0
                name = vals !! 1
                team = vals !! 2
                track = vals !! 3
                skill = vals !! 4

personLine = "10\tMichael\tMobile\tMobilize\tNative"
person = personFromString personLine
