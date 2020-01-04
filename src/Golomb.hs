module Golomb (golomb, try) where

{-  Golomb (difficulty 3.1) - https://open.kattis.com/problems/golombrulers -}

import Control.Arrow  ((>>>))
import Data.List      (group, sort, (\\))
import Scanner


{- Types -}

type Mark      = Int
type TestCase  = [Mark]


-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = NotARuler
               | Perfect
               | Missing [Int]

instance Show Output where
  show (NotARuler ) = "not a ruler"
  show (Perfect   ) = "perfect"
  show (Missing ns) = "missing " ++ unwords (map show ns)


{- Parsing -}

{-  0 1 2
    0 3 1
-}
parseInput :: Scanner [TestCase]
parseInput = many (parseLine $ many int)


{- Methods -}

doCase :: TestCase -> Output
doCase markers = result
  where
    -- find differences between all pairs of markers
    diffs  = sort [ j-i | i <- markers,
                          j <- markers,
                          j > i ]

    result
      | diffs /= unique diffs = NotARuler        -- duplicates found
      | diffs == [1..largest] = Perfect
      | otherwise             = Missing missing
      where
        missing = [1..largest] \\ diffs
        largest = maximum markers


    -- get the unique elements in a list
    --
    -- note, the incoming list here is already sorted, so I removed the sort step to see
    -- how much faster the code would run, but it's actually *slower* for some reason,
    -- using 1.92s without the sort and 1.85s with it.  mind = boggled
    unique :: Ord a => [a] -> [a]
    unique = sort >>> group >>> map head



{- Operations -}

golomb :: String -> String
golomb = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = golomb <$> readFile "test/inputs/golomb.input" >>= putStr

