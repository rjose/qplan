module Filters.QPlanApp (filterString) where

import Data.List
import Data.Maybe
import Text.JSON
import Control.Applicative
import qualified Data.Set as Set

import StackStream
import Work
import Person
import SkillAmount

type SkillName = String
type TrackName = String
type TrackWork =  [[Work]] -- List of tracks, each with a list of work
type TrackStaff = [[[Person]]] -- List of tracks, each with a list of skill teams
type TrackManpower = [[Float]] -- List of tracks, each with list of skills manpower
type TrackDemand = [[[Float]]] -- Tracks, each with a triage list, each with skills manpower
type TrackAvail = TrackDemand -- Same size and shape as TrackDemand
type TrackFeasibility = [[Bool]] -- List of tracks, each with bool list corresp. to worklist

filterString :: String -> String
filterString s = if any isNothing [workStream, staffStream]
                        then ""
                        else result
        where
                streams = unstack $ lines s

                workStream = find (("Work" ==) . header) streams
                staffStream = find (("Staff" ==) . header) streams

                numWeeks = 13
                workItems = map workFromString $ content $ fromJust workStream
                staff = map personFromString $ content $ fromJust staffStream

                tracks = getTracks staff workItems
                skills = getSkills staff workItems

                trackWork = workByTrack tracks workItems
                trackStaff = staffByTrackSkills tracks skills staff

                trackManpower = getManpower (\p -> numWeeks) trackStaff
                trackDemand = getTrackDemand trackWork skills
                trackAvail = getNetAvail trackManpower trackDemand
                trackFeasibility = getTrackFeasibility skills trackManpower trackWork

                result = encode $ makeObj [
                  ("tracks", stringsToJSON tracks),
                  ("skills", stringsToJSON skills),
                  ("triages", stringsToJSON $ map show [P1, P1_5, P2, P2_5, P3]),
                  ("track_stats", trackStatsToJSON trackManpower trackDemand trackAvail)
                                          ]



getTracks :: [Person] -> [Work] -> [TrackName]
getTracks staff workItems = result
        where
                workTracks = Set.fromList $ map Work.track workItems
                staffTracks = Set.fromList $ map Person.track staff
                result' = Set.toList $ Set.union workTracks staffTracks
                result = "All":(sort result')

getSkills :: [Person] -> [Work] -> [SkillName]
getSkills staff workItems = result
        where
                estimates = concat $ map Work.estimate workItems
                skillsDemanded = Set.fromList $ map skill' estimates
                skillsAvailable = Set.fromList $ map Person.skill staff
                result' = Set.toList $ Set.union skillsDemanded skillsAvailable
                result = sort $ filter (\s -> s /= "") result'


workByTrack :: [TrackName] -> [Work] -> TrackWork
workByTrack (all:tracks) ws = result
        where
                result = map (sortBy (\l r -> rank l `compare` rank r)) $ ws:result'
                result' = (\t -> filter (\w -> t == Work.track w) ws) <$> tracks


staffByTrackSkills :: [TrackName] -> [SkillName] -> [Person] -> TrackStaff
staffByTrackSkills (all:tracks) skills staff = result
        where
          staffByTrack' = map (\t -> filter (\p -> t == Person.track p) staff) tracks
          staffByTrack = staff:staffByTrack'
          groupBySkills ps = map (\s -> filter (\p -> s == Person.skill p) ps) skills
          result = map groupBySkills staffByTrack


getManpower :: (Person -> Float) -> TrackStaff -> TrackManpower
getManpower scale staff = result
        where
                result = [trackPower | trackStaff <- staff,
                          let trackPower = map (foldr (\p a -> a + scale p) 0) trackStaff]

getTrackDemand :: TrackWork -> [SkillName] -> TrackDemand
getTrackDemand trackWork skills = result
        where
                result = [trackDemand | ws <- trackWork,
                           let triagedWork = map (selectTriage ws) triages
                               trackDemand' = map (map (getMp skills)) triagedWork
                               trackDemand'' = map sumManpower trackDemand'
                               trackDemand''' = map (conditionDemand len) trackDemand''
                               trackDemand = accManpower trackDemand'''
                         ]
                selectTriage work tri = filter (\w -> tri == triage w) work
                triages = [P1, P1_5, P2, P2_5, P3]
                getMp = flip getWorkManpower
                len = length skills


conditionDemand :: Int -> [Float] -> [Float]
conditionDemand len demand = result
        where
                result = if demand == []
                         then take len $ repeat 0
                         else demand

accManpower :: [[Float]] -> [[Float]]
accManpower [] = []
accManpower (m:ms) = scanl (\acc mp -> sumManpower [acc, mp]) m ms

sumManpower :: [[Float]] -> [Float]
sumManpower [] = []
sumManpower (m:ms) = result
        where
                result = foldl (\acc mp -> zipWith (+) acc mp) m ms


-- TODO: Move this to Work.hs
getWorkManpower :: Work -> [SkillName] -> [Float]
getWorkManpower work skills = result
        where
                result = [manpower | s <- skills,
                           let estimates = estimate work
                               manpower' = find (\e -> s == skill' e) estimates
                               manpower = if isNothing manpower'
                                          then 0
                                          else numval' $ fromJust manpower'
                         ]

getNetAvail :: TrackManpower -> TrackDemand -> TrackAvail
getNetAvail manpower demand = result
        where
                result = zipWith (\mp d -> map (zipWith (-) mp) d) manpower demand

getTrackFeasibility :: [SkillName] -> TrackManpower -> TrackWork -> TrackFeasibility
getTrackFeasibility skills manpower trackWork = result
        where
                getMp = flip getWorkManpower
                netAvail mp ds = tail $ scanl (zipWith (-)) mp ds
                isFeasibile as ds = and $
                    zipWith (\a d -> if d > 0 && a < 0 then False else True) as ds
                trackDemand = [map (getMp skills) ws| ws <- trackWork]
                trackAvail = zipWith netAvail manpower trackDemand
                result = zipWith (zipWith isFeasibile) trackAvail trackDemand



trackStatsToJSON :: TrackManpower -> TrackDemand -> TrackAvail -> JSValue
trackStatsToJSON manpower demand avail = result
        where
                result = makeObj [("manpower", manpower'),
                                  ("demand", demand'),
                                  ("net_avail", netAvail')]
                manpower' = JSArray [floatsToJSON mp | mp <- manpower]
                demand' = JSArray [JSArray $ map floatsToJSON d | d <- demand]
                netAvail' = JSArray [JSArray $ map floatsToJSON a | a <- avail]

stringsToJSON :: [String] -> JSValue
stringsToJSON strings = JSArray $ map (JSString . toJSString) strings

floatsToJSON :: [Float] -> JSValue
floatsToJSON floats = JSArray $ map (JSRational False . toRational) floats




workToJSValue :: Work -> JSValue
workToJSValue w = makeObj [
        ("estimate", JSString $ toJSString $ format $ estimate w),
        ("feasible", JSBool True),
        ("name", JSString $ toJSString $ Work.name w),
        ("rank", JSRational False $ toRational $ rank w),
        ("track", JSString $ toJSString $ Work.track w),
        ("triage", JSString $ toJSString $ show $ triage w)
        ]
        where
                format estimates = intercalate ", " $ map show estimates

personToJSValue :: Person -> JSValue
personToJSValue p = makeObj [
        ("name", JSString $ toJSString $ Person.name p)
        ]


-- Test functions
test = do
        content <- readFile "_qplan.txt"
        let result = filterString content
        putStr result
