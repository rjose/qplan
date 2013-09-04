-----------------------------------------------------------------------------
-- |
-- Module      :  QPlan 
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
-- 
-- Maintainer  :  rjose@ejorp.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides ability to consume structured data from stdin and generate text
-- reports, data for vending machine apps, or data for broadcast for quarterly
-- planning applications.
--
-----------------------------------------------------------------------------

module Main (main) where
 
import System.Environment
import System.Exit
import Data.Maybe
import Data.List
import System.Console.GetOpt
import qualified StackStream as StackStream
import qualified Work as Work
import SkillAmount
import Text.Printf

data Flag
        = Chart String
        | Raw
        | Data
        | Sample
          deriving (Eq, Show)

options :: [OptDescr Flag]
options = 
        [ Option ['c'] ["chart"] (ReqArg chart "CHART") "construct chart data",
          Option ['r'] ["raw"] (NoArg Raw) "generate raw output",
          Option ['d'] ["data"] (NoArg Data) "show data",
          Option ['s'] ["sample"] (NoArg Sample) "sample output"
          ] 

chart :: String -> Flag
chart = Chart


-- | Parses commandline arguments and filters data from stdin to stdout.
main = getArgs >>= run

run :: [String] -> IO ()
run args = do
                contents <- getContents
                putStr $ computeResult flags contents
        where
                (flags, _, _) = getOpt Permute options args

computeResult :: [Flag] -> String -> String
computeResult flags contents
        | Raw `elem` flags = contents
        | Data `elem` flags = unstack' contents
        | Sample `elem` flags = sample_filter_work contents
        | otherwise = "TODO: Handle nothing\n" 

data Stream = Start | Stream String [String]
        deriving Show

-- TODO: Have this return a type class instead
-- TODO: Figure out how to comment Haskell code
getField :: String -> Maybe String
getField "=====title" = Just "title"
getField "=====demand" = Just "demand"
getField "=====shortage" = Just "shortage"
getField _  = Nothing

-- TODO: Make this monadic?
-- TODO: Handle case where first element is not a header
getStream :: [String] -> (Stream, [String])
getStream (s:ss) = (Stream (fromJust $ getField s) $ stream_lines, snd parts)
        where
                parts = break (isJust . getField) ss
                -- Remove leading tab from stream data
                stream_lines = map tail $ fst parts 

getStreams :: [String] -> [Stream]
getStreams [] = []
getStreams ss = stream:rest
        where
                (stream, rest_strs) = getStream ss
                rest = getStreams rest_strs

unstack' :: String -> String
unstack' input = unlines shortage_data
        where
                streams = getStreams . lines $ input
                (Stream _ shortage_data) = streams !! 2


-- TODO: Figure out where to move this
sample_filter_work :: String -> String
sample_filter_work s = result
        where
                streams = StackStream.unstack $ lines s
                work_stream = find (("Work" ==) . StackStream.header) streams
                result = if isNothing work_stream then "" else result'
                StackStream.Stream _ ls = fromJust work_stream
                work_items = map Work.fromString ls
                result' = work_filter1 work_items

work_filter1 :: [Work.Work] -> String
work_filter1 ws = unlines $ map (\w -> printf "%s\t%s"
        (Work.name w :: String) (show $ Work.estimate w :: String)) ws
