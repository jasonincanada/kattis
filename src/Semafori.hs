module Semafori (semafori, try) where

{-  Semafori (difficulty 2.1) - https://open.kattis.com/problems/semafori -}

import Control.Arrow ((>>>))
import Scanner


{- Types -}

type Distance   = Int
type Time       = Int

--                (position on the road, duration of red, duration of green)
type Light      = (Distance, Time, Time)
type RoadLength = Distance

type TestCase   = (RoadLength, [Light])

-- The output object will be different for each challenge.  It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Output Int

instance Show Output where
  show (Output time) = show time


{- Parsing -}

parseInput :: Scanner TestCase
parseInput = do
  roadLength <- parseRoadLength 
  lights     <- many parseLight

  return (roadLength, lights)


{-  4 30  -}
-- Skip over the first word and collect the second one as an integer
parseRoadLength :: Scanner Distance
parseRoadLength = parseLine (str >> int)

-- 7 13 5
parseLight :: Scanner Light
parseLight = parseLine scanner
  where
    scanner = (,,) <$> int -- collect the first three words as integers and product them up into a triple
                   <*> int
                   <*> int


{- Logic -}

-- Calculate the total time required to get to the end of the road while stopping at red lights
measure :: RoadLength -> [Light] -> Time
measure length lights = let (t, d)        = foldl step (0, 0) lights
                            distanceToEnd = length - d
                         in t + distanceToEnd
  where
    step :: (Time, Distance) -> Light -> (Time, Distance)
    step (t, d) (p, r, g) = let
                               distanceToLight  = p - d
                               timeToGetToLight = distanceToLight -- 1 m/s
                               timeAtLight      = t + timeToGetToLight
                               timeLeftOnRed    = max 0 (r - timeAtLight `mod` (r + g))
                               timeToAdd        = timeToGetToLight + timeLeftOnRed
                             in
                               (t + timeToAdd, d + distanceToLight)


doCase :: TestCase -> Output
doCase (length, lights) = Output $ measure length lights


{- Operations -}

semafori :: String -> String
semafori = parse >>> process >>> combine
  where
    parse   = runScanner lines parseInput
    process = doCase >>> show
    combine = id


-- For running in the GHCi repl
try :: IO ()
try = do
  semafori <$> readFile "inputs/semafori-2.input" >>= putStr

