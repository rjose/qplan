module Filters.QPlanApp (filterString) where

import Data.List
import Data.Maybe
import Prelude hiding (filter)
import Text.Printf
import Text.JSON

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
                result' = work_filter2 work_items


workToString :: [Work] -> String
workToString ws = unlines $ map (\w -> printf "%s\t%s"
        (name w :: String) (show $ estimate w :: String)) ws


-- Sample filters
work_filter2 :: [Work.Work] -> String
work_filter2 ws = result
        where
                work_array = [toJSObject [("name", Work.name w), ("track", Work.track w)] | w <- ws ]
                result = encode $ toJSObject [("work", work_array)]

work_filter3 :: [Work.Work] -> String
work_filter3 ws = result
        where
                track_groups = sortBy groupOrder $ groupBy (\l r -> Work.track l == Work.track r) ws
                groupOrder (gl:gls) (gr:grs) = Work.track gl `compare` Work.track gr
                groupOrder [] grs = LT
                groupOrder gls [] = GT
                tracks = map (\g -> Work.track $ head g) track_groups
                result = encode tracks
