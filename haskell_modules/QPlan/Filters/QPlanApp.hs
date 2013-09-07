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

-- Items with a "'" are JSON-ready values
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

                manpower = getManpower (\p -> numWeeks) trackStaff
                trackDemand = getTrackDemand trackWork skills
                netAvail = getNetAvail manpower trackDemand

                result = show $ netAvail !! 3

                -----------------------

                --result = show $ trackDemand
                --result = show $ (tracks !! 3, trackStaff !! 3)
                --result = show staff
                --result = show $ zip tracks manpower
                --result = show skills
                --result = show $ (tracks, trackWork !! 1)


                -- NOTE: Here, it starts getting out of hand
                --work_by_track = groupBy (\l r -> Work.track l == Work.track r) workItems
                --tracks2 = "All":(map (Work.track . head) work_by_track)
                --ranked = [sortBy (\l r -> rank l `compare` rank r) ws |
                --                                ws <- (workItems:work_by_track)]


                -- HERE!
                --staffBySkill = groupBy (\l r -> Person.skill l == Person.skill r) staff
                --skills2 = map (Person.skill . head) staffBySkill
                --staffBySkill' = zip skills2 staffBySkill
                --staffByTrack2 = trackStaff2 tracks2 staffBySkill'
                --staffByTrack2' = ("All", staffBySkill'):(zip tracks2 staffByTrack2)

                --availableStaff = getAvailableStaff (\p -> 1) staffByTrack2'
                --availableManpower = getAvailableStaff (\p -> numWeeks) staffByTrack2'
                --cumulativeDemand = cumulativeDemandByTrackTriage ranked
                --netAvail = netAvailableByTrackTriage availableManpower cumulativeDemand

                ----result = show $ netAvail !! 7

                --staff' = staffToJSValue staffByTrack2'
                ----staffStats' = staffStats availableManpower cumulativeDemand netAvail

                --tracks' = map (JSString . toJSString) tracks2
                --workItems' = processRanked tracks2 ranked

                --result' = encode $ makeObj [("tracks", JSArray tracks'),
                --                            ("work_items", workItems'),
                --                            ("staff", staff')
                --                           ]

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

----------------------

netAvailableByTrackTriage ::
        TrackSkillAmounts -> [[[SkillAmount]]] -> [(TrackName, [[SkillAmount]])]
netAvailableByTrackTriage avail demandByTrackTriage = result
        where
                result = zipWith combine avail demandByTrackTriage
                combine (track, supply) demands =(track, netAvail supply demands)
                netAvail supply demands = skillDifference <$> [supply] <*> demands

workByTrackTriage :: [[Work]] -> [[[Work]]]
workByTrackTriage workGroups = map workWithAllTriages workGroups

workWithAllTriages :: [Work] -> [[Work]]
workWithAllTriages work = workWithTriage <$> [P1, P1_5, P2, P2_5, P3] <*> [work]

cumulativeDemandByTrackTriage :: [[Work]] -> [[[SkillAmount]]]
cumulativeDemandByTrackTriage workGroups = map cumulativeDemandByAllTriages workGroups

cumulativeDemandByAllTriages :: [Work] -> [[SkillAmount]]
cumulativeDemandByAllTriages work = result
        where
                (_:result) = scanl (\acc est -> skillSum $ concat [acc, est]) [] result'
                result' = demandByAllTriages work

demandByAllTriages :: [Work] -> [[SkillAmount]]
demandByAllTriages work = demandByTriage <$> [P1, P1_5, P2, P2_5, P3] <*> [work]

demandByTriage :: Triage -> [Work] -> [SkillAmount]
demandByTriage tri work = filter (\s -> SkillAmount.skill s /= "")  result'
        where
                work' = workWithTriage tri work
                result' = skillSum $ concat $ map estimate work'

workWithTriage :: Triage -> [Work] -> [Work]
workWithTriage tri work = filter (\w -> tri == triage w) work

getSkillDemand :: [Work] -> [SkillAmount]
getSkillDemand work = skillSum $ concat $ map estimate work

getNetAvail2 :: [Work] -> [SkillAmount] -> [[SkillAmount]]
getNetAvail2 work skills = scanl (\s w -> skillDifference s (estimate w)) skills work

getIsFeasibleList :: [Work] -> [SkillAmount] -> [Bool]
getIsFeasibleList work skills = result
        where
                net_avail = scanl (\s w -> skillDifference s (estimate w)) skills work
                result = zipWith (\w s -> isFeasible w s) work net_avail

isFeasible :: Work -> [SkillAmount] -> Bool
isFeasible w skills = and (map (\n -> isSkillValFeasible n skills) estimateNames)
        where
                estimateNames = map SkillAmount.skill' $ estimate w

isSkillValFeasible :: Skill -> [SkillAmount] -> Bool
isSkillValFeasible skillname amounts = result
        where
                skillAmount = find (\s -> skillname == SkillAmount.skill' s) amounts
                value = if isJust skillAmount
                                then fmap numval' skillAmount
                                else Nothing
                result = if isNothing value
                                then False
                                else (> 0) $ fromJust value


type TrackStaff2 = [(TrackName, [(SkillName, [Person])])]
type TrackSkillAmounts = [(TrackName, [SkillAmount])]

getAvailableStaff :: (Person -> Float) -> TrackStaff2 -> TrackSkillAmounts
getAvailableStaff scale staff = result
        where
                result = [(track, availSkills) | (track, skillGroups) <- staff,
                        let availSkills = [(SkillSum skill avail) | (skill, people) <- skillGroups,
                                let avail = foldr (\p a -> a + scale p) 0 people] ]


-- Takes a list of track names and a list of skill/people groupings and
-- returns a list of list of skill/people groupings by track.
trackStaff :: [TrackName] -> [(SkillName, [Person])] -> [ [(SkillName, [Person])] ]
trackStaff tracks groups = filterStaffsByTrack <$> tracks <*> [groups]

filterStaffsByTrack :: TrackName -> [(SkillName, [Person])] -> [(SkillName, [Person])]
filterStaffsByTrack track groups = map (filterStaffByTrack track) groups

filterStaffByTrack :: TrackName -> (SkillName, [Person]) -> (SkillName, [Person])
filterStaffByTrack track (skill, people) = (skill, peopleInTrack)
        where
                peopleInTrack = filter (\p -> Person.track p == track) people

-- This one's a little tricky
staffToJSValue :: [(TrackName, [(SkillName, [Person])])] -> JSValue
staffToJSValue staff = makeObj staff'
        where
               staff' = [(track, makeObj skillGroup') | (track, skillGroup) <- staff,
                        let skillGroup' = [(skill, JSArray people') | (skill, people) <- skillGroup,
                                        let people' = map personToJSValue people]
                        ]


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
