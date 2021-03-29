{-# Language TupleSections #-}

{-  Train Boarding (difficulty 5.4) - https://open.kattis.com/problems/trainboarding -}

module TrainBoarding (trainboarding, try) where

import Data.Function    ((&))
import Data.Traversable (mapAccumR)
import qualified Data.IntMap as M


{- Types -}

type Distance = Int
type Car      = Int

-- number of cars, length of each car, passenger distances from start of train
type TestCase = (Int, Int, [Distance])

data Output   = Output Distance Int

instance Show Output where
  show (Output distance count) = unlines [ show distance,
                                           show count ]


{- Parsing -}

{-  68 90 3
    3809
    288
    2306
-}
parse :: String -> TestCase
parse input = (read n, read l, read <$> distances)
  where
    (nl : distances) = lines input
    (n : l : _)      = words nl



{- Methods -}

-- the farthest we've seen a passenger walk
data Accum = Accum Distance


process :: TestCase -> Output
process (n, l, distances) = Output longest boarding
  where

    -- visit each passenger in the list, determining the nearest car door, 
    -- swapping them in the output list with the car number. meanwhile we
    -- accumulate the longest distance we've seen a passenger walk
    --
    -- mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
    (Accum longest, cars) = mapAccumR visit (Accum 0) distances

    visit :: Accum -> Distance -> (Accum, Car)
    visit (Accum longest) x = (Accum maxWalk, car)
      where
        maxWalk = max longest walk

        walk :: Distance
        walk = abs $ x - xDoor
          where
            xDoor = car * l + (l `div` 2)

        car :: Car
        car = min (n-1) (x `div` l)


    -- second phase, with the list of car numbers entered by passengers, calculate
    -- the maximum number of passengers who entered the same car
    boarding :: Int
    boarding = maximum $ tally cars

    tally :: [Car] -> [Int]
    tally cars = map (,1) cars
                   & M.fromListWith (+)
                   & M.elems



{- Operations -}

trainboarding :: String -> String
trainboarding = show . process . parse

try :: IO ()
try = trainboarding <$> readFile "test/inputs/trainboarding-1.input" >>= putStrLn

