import Data.Maybe

-- TODO: Handle commandline args

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

main = interact unstack

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

unstack :: String -> String
unstack input = unlines shortage_data
        where
                streams = getStreams . lines $ input
                (Stream _ shortage_data) = streams !! 2
