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
                --result = show $ getNetAvail (head ranked) (snd $ head availableManpower)
                result = show $ getIsFeasibleList (head ranked) (snd $ head availableManpower)

                staff' = staffToJSValue staffByTrack'

                tracks' = map (JSString . toJSString) tracks
                ranked' = processRanked tracks ranked
                result' = encode $ makeObj [("tracks", JSArray tracks'),
                                             ("work_items", ranked'),
                                             ("staff", staff')
                                            ]

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
