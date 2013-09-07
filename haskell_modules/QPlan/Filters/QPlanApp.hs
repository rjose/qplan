module Filters.QPlanApp (filterString) where

import Data.List
import Data.Maybe
import Text.JSON
import Control.Applicative

import StackStream
import Work
import Person
import SkillAmount

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

                work_by_track = groupBy (\l r -> Work.track l == Work.track r) work_items
                tracks = "All":(map (Work.track . head) work_by_track)
                ranked = [sortBy (\l r -> rank l `compare` rank r) ws |
                                                ws <- (work_items:work_by_track)]

                staffBySkill = groupBy (\l r -> Person.skill l == Person.skill r) staff
                skills = map (Person.skill . head) staffBySkill
                staffBySkill' = zip skills staffBySkill
                staffByTrack = trackStaff tracks staffBySkill'
                staffByTrack' = ("All", staffBySkill'):(zip tracks staffByTrack)
                numWeeks = 13
                availableStaff = getAvailableStaff (\p -> 1) staffByTrack'
                availableManpower = getAvailableStaff (\p -> numWeeks) staffByTrack'
                --result = show $ getIsFeasibleList (head ranked) (snd $ head availableManpower)
                --result = show $ getSkillDemand (ranked !! 3)
                --result = show $ workWithTriage P2_5 (ranked !! 2)
                --result = show $ workWithAllTriages (ranked !! 3)
                --result = show $ (workByTrackTriage ranked !! 3)
                --result = show $  demandByTriage P1 (ranked !! 3)
                --result = show $ demandByAllTriages (ranked !! 3)
                --result = show $ cumulativeDemandByAllTriages (ranked !! 3)
                --result = show $ cumulativeDemandByTrackTriage ranked
                cumulativeDemand = cumulativeDemandByTrackTriage ranked
                result = show $ (netAvailableByTrackTriage availableManpower cumulativeDemand) !! 7

                staff' = staffToJSValue staffByTrack'

                tracks' = map (JSString . toJSString) tracks
                ranked' = processRanked tracks ranked
                result' = encode $ makeObj [("tracks", JSArray tracks'),
                                             ("work_items", ranked'),
                                             ("staff", staff')
                                            ]

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

getNetAvail :: [Work] -> [SkillAmount] -> [[SkillAmount]]
getNetAvail work skills = scanl (\s w -> skillDifference s (estimate w)) skills work

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


type TrackStaff = [(TrackName, [(SkillName, [Person])])]
type TrackSkillAmounts = [(TrackName, [SkillAmount])]

getAvailableStaff :: (Person -> Float) -> TrackStaff -> TrackSkillAmounts
getAvailableStaff scale staff = result
        where
                result = [(track, availSkills) | (track, skillGroups) <- staff,
                        let availSkills = [(SkillSum skill avail) | (skill, people) <- skillGroups,
                                let avail = foldr (\p a -> a + scale p) 0 people] ]

type SkillName = String
type TrackName = String

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
getStaff = do
        content <- readFile "_qplan.txt"
        let result = filterString content
        putStr result
