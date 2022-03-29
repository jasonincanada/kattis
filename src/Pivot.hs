{-  Pivot (difficulty 2.7) - https://open.kattis.com/problems/pivot

    This solution uses the Tardis monad (combination of forward and reverse state) to determine if
    each element in a list can be a pivot for the list, meaning all numbers to the left are less
    than (or equal) to the pivot, and all numbers to the right are greater. This requires a
    computation at each element of the list based on both the prior elements and the remainder of
    the list, even though we haven't yet visited the later elements. The technique used for this
    solution is very similar to Phil Freeman's water tower solution here:

      https://gist.github.com/paf31/9d84ecf6a6a9b69cdb597a390f25764d

    The difference is that the elements to the left of the pivot in this problem have to be smaller
    than the pivot, and the elements to the right have to be greater. So in the forwards (normal)
    direction, we track the largest number we've seen (with `max`) to make sure the pivot is bigger
    than it. In the backwards direction, we track the minimum number to make sure it's larger than
    the pivot. This occurs succinctly in only a few lines in the `check` function

    This code passes the two sample inputs but I don't know if it passes the kattis server tests
    because the Tardis package is not installed on the servers

    Note: I think the water tower problem that Freeman solves in the gist above is basically the
    same as the Bungee Builder problem on the kattis site below. His solution could probably be used
    to answer that challenge, though I haven't tried it

      https://open.kattis.com/problems/bungeebuilder

-}

module Pivot (pivot, try) where

import Control.Monad.Tardis


{- Types -}

type TestCase = [Int]
data Output   = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

-- 8
-- 2 1 3 4 7 5 6 8
parse :: String -> TestCase
parse = map read . words . head . tail . lines


{- Methods -}

process :: TestCase -> Output
process nums = Output count
  where

    -- the result of running tardis on the list of ints is a list of
    -- bools, signifying whether each element could have been a pivot
    pivots = evalTardis (traverse check nums) start
    count  = length $ filter id pivots

    -- tardis function here
    check :: Int -> Tardis Int Int Bool
    check num = do

      modifyForwards  (`max` num)
      modifyBackwards (`min` num)

      p <- getPast
      f <- getFuture

      pure $  p <= num && num < f   -- this is a pivot if it's between the prior
                                    -- maximum and the future minimum


    start :: (Int, Int)
    start = (maxBound, minBound)


{- Operations -}

pivot :: String -> String
pivot = show . process . parse

try :: IO ()
try = pivot <$> readFile "test/inputs/pivot-1.input" >>= putStrLn

