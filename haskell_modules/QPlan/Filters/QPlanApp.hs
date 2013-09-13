-----------------------------------------------------------------------------
-- |
-- Module      :  Filters.QPlanApp
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
--
-- Maintainer  :  @rjose
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts work and staff streams into JSON data appropriate for QPlan app.
-- This enables analysis of staff skill shortages by triage.
--
-----------------------------------------------------------------------------
module Filters.QPlanApp (filterString) where

import Control.Applicative
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import qualified Data.Set as Set
import System.Locale

import Filters.Schedule.Internal
import Filters.Utils
import Person
import SkillAmount
import StackStream
import Work

-- =============================================================================
-- Data types
--
type SkillName = String
type TrackName = String
type TrackWork =  [[Work]] -- List of tracks, each with a list of work
type TrackStaff = [[[Person]]] -- List of tracks, each with a list of skill teams
type TrackManpower = [[Float]] -- List of tracks, each with list of skills manpower
type TrackDemand = [[[Float]]] -- Tracks, each with a triage list, each with skills manpower
type TrackAvail = TrackDemand -- Same size and shape as TrackDemand
type TrackFeasibility = [[Bool]] -- List of tracks, each with bool list corresp. to worklist


-- =============================================================================
-- Public API
--

--------------------------------------------------------------------------------
-- | Converts qplan input streams into result streams for qplan.
--
filterString :: String -> String
filterString s = if any isNothing [workStream, staffStream, holidayStream,
                                   paramStream]
                        then error "One of the input streams was missing"
                        else result
        where
                -- Unstack the input streams
                streams = unstack $ lines s
                workStream = find (("qplan work v1" ==) . header) streams
                staffStream = find (("qplan staff v1" ==) . header) streams
                holidayStream = find (("qplan holidays v1" ==) . header) streams
                paramStream = find (("qplan params v1" ==) . header) streams

                -- Parse info out of streams
                (startDate, endDate, numWeeks, schedSkillStr) =
                                                getParams $ fromJust paramStream
                holidays = map stringToDay $ content $ fromJust holidayStream
                workItems = map workFromString $ content $ fromJust workStream
                staff = sort $ map personFromString $ content $ fromJust staffStream

                -- Define "master lists"
                --      Any list organized by track, skill, or time will
                --      correspond directly to these lists.
                tracks = getTracks staff workItems
                skills = getSkills staff workItems
                days = getDays startDate endDate
                triages = map show [P1 .. P3]

                -- Group work and staff into tracks
                trackWork = workByTrack tracks workItems
                trackStaff = staffByTrackSkills tracks skills staff

                -- Compute resource demand and feasibility
                trackManpower = getManpower (\p -> numWeeks) trackStaff
                trackDemand = getTrackDemand trackWork skills
                trackFeasibility = getTrackFeasibility skills trackManpower trackWork

                -- TODO: Tighten the rest of this code up
                -- Schedule work based on track assignments
                schedSkills = splitOn ":" schedSkillStr
                workDays = map (isWorkDay holidays) days
                trackStaffAvail = getTrackStaffAvail skills schedSkills workDays trackStaff
                trackDates = getTrackWorkDates schedSkills trackWork days trackStaffAvail

                -- Generate result
                -- TODO: Have StacKStream do the stream construction for us
                trackStream = ["=====qplan tracks v1"] ++ (map addTab tracks)
                skillStream = ["=====qplan skills v1"] ++ (map addTab skills)
                triageStream = ["=====qplan triages v1"] ++ (map addTab triages)

                manpowerStream = ["=====qplan track manpower v1"] ++
                        [addTab l | mp <- trackManpower,
                                let l = joinWith "\t" $ map show mp]

                trackDemandStream = ["=====qplan track-triage demand v1"] ++
                        map addTab (concat [getTriageStream d | d <- trackDemand])

                trackStaffStream = ["=====qplan track-skill staff v1"] ++
                        map addTab (concat [getTrackGroupStream s | s <- trackStaff])

                trackWork' = zip3 trackWork trackFeasibility trackDates
                trackWorkStream = ["=====qplan track work v1"] ++
                        map addTab (concat [getWorkStream ws | ws <- trackWork'])

                -- TODO: See if there's a more Haskell-y way to do this
                result = unlines $ concat [packStream $ fromJust paramStream,
                                           trackStream, skillStream, triageStream,
                                           manpowerStream, trackDemandStream,
                                           trackStaffStream, trackWorkStream]


-- =============================================================================
-- Internal functions
--

getWorkStream :: ([Work], [Bool], [Maybe Day]) -> [String]
getWorkStream (ws, fs, ds) = result
        where
                result = ["=====qplan track item"] ++
                        map addTab (zipWith3 format ws fs ds)
                format w f d = joinWith "\t" [Work.track w, show $ rank w, show $ triage w,
                                              Work.name w, formatEstimate $ estimate w,
                                              show f, formatDay d]
                formatDay d = if isNothing d
                                then "DNF"
                                else dayToString $ fromJust d



getTrackGroupStream :: [[Person]] -> [String]
getTrackGroupStream skillGroup = result
        where
                result = ["=====qplan track item"] ++
                        map addTab (concat [getSkillStream people | people <- skillGroup])

getSkillStream :: [Person] -> [String]
getSkillStream people = result
        where
                result = ["=====qplan skill item"] ++
                        map addTab [Person.name p | p <- people]

packStream :: Stream -> [String]
packStream (Stream header content) = result
        where
                result = ["=====" ++ header] ++ (map addTab content)

getTriageStream :: TrackManpower -> [String]
getTriageStream manpower = result
        where
                result = ["=====qplan triage item"] ++
                        [addTab l | mp <- manpower,
                                let l = joinWith "\t" $ map show mp]


-- TODO: Move some of these functions to Utils
joinWith :: String -> [String] -> String
joinWith _ [] = []
joinWith _ [w] = w
joinWith c (w:ws) = w ++ c ++ (joinWith c ws)


addTab :: String -> String
addTab s = "\t" ++ s

getParams :: Stream -> (Day, Day, Float, String)
getParams (Stream _ ls) = (startDate, endDate, (fromInteger $ round numWeeks), schedStr)
        where
                params = splitOn "\t" (head ls)
                startDate = stringToDay $ params !! 0
                endDate = stringToDay $ params !! 1
                schedStr = params !! 2
                numWeeks = (/ 7.0) $ fromInteger ((diffDays endDate startDate) + 1) :: Float


getTrackWorkDates :: [SkillName] -> TrackWork -> [Day] -> [SkillAvailabilities] ->
                     [[Maybe Day]]
getTrackWorkDates skills trackWork days trackStaffAvail = result
        where
                schedAvails = map (\avail -> (days, avail)) trackStaffAvail
                result = zipWith (schedule skills) trackWork schedAvails



getTrackStaffAvail :: [SkillName] -> [SkillName] -> [(Day, Bool)] -> TrackStaff ->
                      [SkillAvailabilities]
getTrackStaffAvail allSkills skills isWorkdays trackStaff = result
        where
                result = [avail | skillGroups <- trackStaff,
                                let
                                skillGroups' = filterSkills allSkills skills skillGroups
                                getAvail = sumAvailability . map (getAvailability isWorkdays)
                                avail = map getAvail skillGroups'
                         ]

filterSkills :: [SkillName] -> [SkillName] -> [a] -> [a]
filterSkills allSkills selectSkills items = result
        where
                pairs = zip allSkills items
                f p acc = if (fst p) `elem` selectSkills
                                then (snd p):acc
                                else acc
                result = foldr f [] pairs


--------------------------------------------------------------------------------
-- Takes union of tracks from a list of people and work items.
--
--      NOTE: This also prepends the "All" track to represent items from all
--      tracks.
--
getTracks :: [Person] -> [Work] -> [TrackName]
getTracks staff workItems = result
        where
                workTracks = Set.fromList $ map Work.track workItems
                staffTracks = Set.fromList $ map Person.track staff
                result' = Set.toList $ Set.union workTracks staffTracks
                result = "All":(sort result')

--------------------------------------------------------------------------------
-- Takes union of skills from a list of people and work items.
--
getSkills :: [Person] -> [Work] -> [SkillName]
getSkills staff workItems = result
        where
                estimates = concat $ map Work.estimate workItems
                skillsDemanded = Set.fromList $ map skill' estimates
                skillsAvailable = Set.fromList $ map Person.skill staff
                result' = Set.toList $ Set.union skillsDemanded skillsAvailable
                result = sort $ filter (\s -> s /= "") result'


--------------------------------------------------------------------------------
-- Groups work items by specified track names.
--
--      NOTE: All of the work items "ws" are prepended to the result. This
--      corresponds to the "All" track.
--
workByTrack :: [TrackName] -> [Work] -> TrackWork
workByTrack (all:tracks) ws = result
        where
                result' = (\t -> filter (\w -> t == Work.track w) ws) <$> tracks
                result = map (sortBy (\l r -> rank l `compare` rank r)) $ ws:result'


--------------------------------------------------------------------------------
-- Organizes staff by track and then by skill within each track.
--
--      The track names and skills in question are provided as arguments. In
--      effect, this allows us to assume the order of items in lists is
--      meaningful (e.g., the first item here corresponds to the first track).
--
staffByTrackSkills :: [TrackName] -> [SkillName] -> [Person] -> TrackStaff
staffByTrackSkills (all:tracks) skills staff = result
        where
          staffByTrack' = map (\t -> filter (\p -> t == Person.track p) staff) tracks
          staffByTrack = staff:staffByTrack'
          groupBySkills ps = map (\s -> filter (\p -> s == Person.skill p) ps) skills
          result = map groupBySkills staffByTrack


--------------------------------------------------------------------------------
-- Computes manpower for track staff.
--
--      This also uses a "scale" function to provide info on resource
--      availability. For instance, this would typically map people to 13 (for
--      13 weeks in a quarter).
--
getManpower :: (Person -> Float) -> TrackStaff -> TrackManpower
getManpower scale staff = result
        where
                result = [trackPower | trackStaff <- staff,
                          let trackPower = map (foldr (\p a -> a + scale p) 0) trackStaff]


--------------------------------------------------------------------------------
-- Computes manpower requirements for track work items.
--
--      Track work is divided into triage groups. The demand is reported as a
--      running total across the triage groups, from high to low priority.
--
--      The resulting manpower skill requirements will be returned in the same
--      order as the "skills" list. If a skill is not required for a work item,
--      0 will be returned as the skill requirement.
--
getTrackDemand :: TrackWork -> [SkillName] -> TrackDemand
getTrackDemand trackWork skills = result
        where
                result = [trackDemand | ws <- trackWork,
                           let triagedWork = map (selectTriage ws) triages
                               trackDemand' = map (map (getWorkManpower skills)) triagedWork
                               trackDemand'' = map sumManpower trackDemand'
                               trackDemand''' = map (conditionDemand len) trackDemand''
                               trackDemand = accManpower trackDemand'''
                         ]
                selectTriage work tri = filter (\w -> tri == triage w) work
                triages = [P1 .. P3]
                len = length skills


--------------------------------------------------------------------------------
-- Maps an empty demand array into one with a specified number of zeroes.
--
conditionDemand :: Int -> [Float] -> [Float]
conditionDemand len demand = result
        where
                result = if demand == []
                         then take len $ repeat 0
                         else demand

--------------------------------------------------------------------------------
-- Accumulates manpower totals.
--
accManpower :: [[Float]] -> [[Float]]
accManpower [] = []
accManpower (m:ms) = scanl (\acc mp -> sumManpower [acc, mp]) m ms


--------------------------------------------------------------------------------
-- Sums list of manpower items.
--
--      NOTE: The position in the manpower arrays corresponds to whatever skills
--      array was used in the calling functions.
--
sumManpower :: [[Float]] -> [Float]
sumManpower [] = []
sumManpower (m:ms) = result
        where
                result = foldl (\acc mp -> zipWith (+) acc mp) m ms


--------------------------------------------------------------------------------
-- Computes feasibility of track work.
--
--      This checks the net availability across all items in all tracks and
--      returns corresponding Bool values if any of the required skills for an
--      item have been exhausted.
--
getTrackFeasibility :: [SkillName] -> TrackManpower -> TrackWork -> TrackFeasibility
getTrackFeasibility skills manpower trackWork = result
        where
                netAvail mp ds = tail $ scanl (zipWith (-)) mp ds
                isFeasibile as ds = and $
                    zipWith (\a d -> if d > 0 && a < 0 then False else True) as ds
                trackDemand = [map (getWorkManpower skills) ws| ws <- trackWork]
                trackAvail = zipWith netAvail manpower trackDemand
                result = zipWith (zipWith isFeasibile) trackAvail trackDemand

test = do
        content <- readFile "_qplan.txt"
        let result = filterString content
        putStr result
