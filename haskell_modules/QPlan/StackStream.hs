module StackStream (
        unstack,
        streamHeader
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

fromString :: String -> Maybe Header
fromString s
        | "=====" `isPrefixOf` s = Just $ Header $ drop prefixLen s
        | otherwise = Nothing
                where prefixLen = 5

streamHeader :: Stream -> String
streamHeader (Stream (Header h) _) = h
streamHeader _ = ""

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
