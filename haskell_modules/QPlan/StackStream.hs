-----------------------------------------------------------------------------
-- |
-- Module      :  StackStream
-- Copyright   :  (c) Rino Jose 2013
-- License     :  BSD-style
-- 
-- Maintainer  :  rjose@ejorp.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides ability to work with "stacked streams" to separate them into
-- component streams or to create stacked streams from different sources.
--
-----------------------------------------------------------------------------

module StackStream (
        unstack,
        Stream(..),
        Header(..)
) where

import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad

-- | Provides description of stream data and how to interpret it.
data Header
        = Header String
        deriving (Show)

-- | Component of stacked stream.
data Stream
        = Stream Header [String]
        | EmptyStream
        deriving (Show)

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

-- | Translates a stacked stream into its components.
unstack :: [String] -> [Stream]
unstack [] = []
unstack ss = stream:unstack rest
        where
                (stream, rest) = getStream ss
