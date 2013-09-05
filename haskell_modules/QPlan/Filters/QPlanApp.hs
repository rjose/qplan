module Filters.QPlanApp (filterString) where

import Data.List
import Data.Maybe
import Prelude hiding (filter)
import Text.Printf

import StackStream
import Work


filterString :: String -> String
filterString s = result
        where
                streams = unstack $ lines s
                work_stream = find (("Work" ==) . header) streams
                result = if isNothing work_stream then "" else result'
                Stream _ ls = fromJust work_stream
                work_items :: [Work]
                work_items = map workFromString ls
                result' = workToString work_items


workToString :: [Work] -> String
workToString ws = unlines $ map (\w -> printf "%s\t%s"
        (name w :: String) (show $ estimate w :: String)) ws
