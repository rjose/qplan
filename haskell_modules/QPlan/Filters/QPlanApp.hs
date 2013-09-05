module Filters.QPlanApp (filterString) where

import Data.List
import Data.Maybe
import Prelude hiding (filter)
import Text.Printf
import Text.JSON

import StackStream
import Work


-- TODO: Add support for Staff stream
filterString :: String -> String
filterString s = if any isNothing [work_stream, staff_stream]
        then ""
        else result
        where
                streams = unstack $ lines s
                work_stream = find (("Work" ==) . header) streams
                staff_stream = find (("Work" ==) . header) streams
                work_items = map workFromString $ content $ fromJust work_stream
                work_by_track = groupBy (\l r -> track l == track r) work_items
                tracks = "All":(map (track . head) work_by_track)
                tracks' = map (JSString . toJSString) tracks
                ranked = [sortBy (\l r -> rank l `compare` rank r) ws | ws <- (work_items:work_by_track)]
                ranked' = processRanked tracks ranked
                result = encode $ makeObj [("tracks", JSArray tracks'), ("work_items", ranked')]



-- NOTE: This will take staff info, too
processRanked :: [String] -> [[Work]] -> JSValue
processRanked tracks ranked = result
        where
                work_group_vals = [JSArray $ map workToJSValue ws | ws <- ranked]
                result = makeObj $ zip tracks work_group_vals

workToJSValue :: Work -> JSValue
workToJSValue w = makeObj [
        ("estimate", JSString $ toJSString $ show $ estimate w),
        ("feasible", JSBool True),
        ("name", JSString $ toJSString $ name w),
        ("rank", JSRational False $ toRational $ rank w),
        ("track", JSString $ toJSString $ track w),
        ("triage", JSString $ toJSString $ show $ triage w)
        ]

getTracks :: [[Work]] -> [String]
getTracks = map (track . head)

f1 :: String -> [[Work]]
f1 s = result
        where
                streams = unstack $ lines s
                work_stream = find (("Work" ==) . header) streams
                work_items = map workFromString $ content $ fromJust work_stream
                result = groupBy (\l r -> track l == track r) work_items

--                result = constructResult work_stream staff_stream
--                result = if isNothing work_stream then "" else result'
--                Stream _ ls = fromJust work_stream
--                work_items :: [Work]
--                work_items = map workFromString ls
--                result' = work_filter2 work_items

--workToString :: [Work] -> String
--workToString ws = unlines $ map (\w -> printf "%s\t%s"
--        (name w :: String) (show $ estimate w :: String)) ws
--
--
---- Sample filters
--work_filter2 :: [Work.Work] -> String
--work_filter2 ws = result
--        where
--                work_array = [toJSObject [("name", Work.name w), ("track", Work.track w)] | w <- ws ]
--                result = encode $ toJSObject [("work", work_array)]
--
--work_filter3 :: [Work.Work] -> String
--work_filter3 ws = result
--        where
--                track_groups = sortBy groupOrder $ groupBy (\l r -> Work.track l == Work.track r) ws
--                groupOrder (gl:gls) (gr:grs) = Work.track gl `compare` Work.track gr
--                groupOrder [] grs = LT
--                groupOrder gls [] = GT
--                tracks = map (\g -> Work.track $ head g) track_groups
--                result = encode tracks
