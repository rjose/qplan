module Filters.QPlanApp (filterString) where

import Data.List
import Data.Maybe
import Text.JSON

import StackStream
import Work
import Person

-- TODO: Add support for Staff stream
filterString :: String -> String
filterString s = if any isNothing [work_stream, staff_stream]
                 then ""
                 else result
        where
                streams = unstack $ lines s

                work_stream = find (("Work" ==) . header) streams
                staff_stream = find (("Staff" ==) . header) streams

                work_items = map workFromString $ content $ fromJust work_stream
                staff = map personFromString $ content $ fromJust staff_stream

                work_by_track = groupBy (\l r -> track l == track r) work_items
                tracks = "All":(map (track . head) work_by_track)
                ranked = [sortBy (\l r -> rank l `compare` rank r) ws |
                                                ws <- (work_items:work_by_track)]

                tracks' = map (JSString . toJSString) tracks
                ranked' = processRanked tracks ranked
                result = encode $ makeObj [("tracks", JSArray tracks'),
                                           ("work_items", ranked')]

-- NOTE: This will take staff info, too
processRanked :: [String] -> [[Work]] -> JSValue
processRanked tracks ranked = result
        where
                work_group_vals = [JSArray $ map workToJSValue ws | ws <- ranked]
                result = makeObj $ zip tracks work_group_vals

workToJSValue :: Work -> JSValue
workToJSValue w = makeObj [
        ("estimate", JSString $ toJSString $ format $ estimate w),
        ("feasible", JSBool True),
        ("name", JSString $ toJSString $ name w),
        ("rank", JSRational False $ toRational $ rank w),
        ("track", JSString $ toJSString $ track w),
        ("triage", JSString $ toJSString $ show $ triage w)
        ]
        where
                format estimates = intercalate ", " $ map show estimates
