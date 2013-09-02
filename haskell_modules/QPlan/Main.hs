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
-- reports, data for vending machine apps, or data for broadcast.
--
-----------------------------------------------------------------------------
 
import System.Environment
import System.Exit
import Data.Maybe
import System.Console.GetOpt
import StackStream


input="=====title\n" ++
      "\tShortage Chart (with shortage)\n" ++
      "=====demand\n" ++
      "\tTrack 1\t20\n" ++
      "\tTrack 2\t35\n" ++
      "=====shortage\n" ++
      "\tApps\t5\n" ++
      "\tNative\t3\n" ++
      "\tQA\t1"

linput = lines input

data Flag
        = Chart String
        | Raw
        | Data
          deriving (Eq, Show)

options :: [OptDescr Flag]
options = 
        [ Option ['c'] ["chart"] (ReqArg chart "CHART") "construct chart data",
          Option ['r'] ["raw"] (NoArg Raw) "generate raw output",
          Option ['d'] ["data"] (NoArg Data) "show data"
          ] 

chart :: String -> Flag
chart = Chart


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
