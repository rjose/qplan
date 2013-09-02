module StackStream (
        unstack
) where

import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad

data Header
        = Header String
        deriving (Show)

data Stream
        = Stream Header [String]
        | EmptyStream
        deriving (Show)

-- TODO: Look into using the State monad when unstacking the stream
fromString :: String -> Maybe Header
fromString s
        | "=====" `isPrefixOf` s = Just $ Header $ drop prefixLen s
        | otherwise = Nothing
                where prefixLen = 5

nextState :: State [String] Stream
nextState = state $ getStream

getStream :: [String] -> (Stream, [String])
getStream (s:ss) = (stream, rest)
        where
                header = fromJust $ fromString s
                (sdata, rest) = break (isJust . fromString) ss 
                stream = Stream header $ map tail sdata
getStream [] = (EmptyStream, [])

unstack :: [String] -> [Stream]
unstack [] = []
unstack ss = stream:unstack rest
        where
                (stream, rest) = getStream ss


-- DATA

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
--unstack :: String -> [Stream]
--unstack input = unlines shortage_data
--        where
--                streams = getStreams . lines $ input
--                (Stream _ shortage_data) = streams !! 2
--
--
--getStream :: [String] -> (Stream, [String])
--getStream (s:ss) = (Stream (fromJust $ getField s) $ stream_lines, snd parts)
--        where
--                parts = break (isJust . getField) ss
--                -- Remove leading tab from stream data
--                stream_lines = map tail $ fst parts 
--
--getStreams :: [String] -> [Stream]
--getStreams [] = []
--getStreams ss = stream:rest
--        where
--                (stream, rest_strs) = getStream ss
--                rest = getStreams rest_strs
