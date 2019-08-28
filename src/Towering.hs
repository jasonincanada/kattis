module Towering (towering, try) where

{-  Towering (difficulty 2.1) - https://open.kattis.com/problems/towering -}

import Control.Arrow ((>>>))
import Data.List     (sortBy, (\\))
import Scanner


{- Types -}

type Height     = Int
type Box        = Height  -- represent a box by its height
type TestCase   = ([Box], Height, Height)

-- The output object will be different for each challenge.  It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Output [Box] [Box]

instance Show Output where
  show (Output left right) = unwords [unwords $ map show left,
                                      unwords $ map show right]


{- Parsing -}

{- 12 8 2 4 10 3 25 14 -}
parseInput :: Scanner TestCase
parseInput = (,,) <$> six int  -- the box heights
                  <*> int      -- height of the left tower
                  <*> int      -- height of the right tower


{- Logic -}

doCase :: TestCase -> Output
doCase (boxes, left, _) = Output leftTower rightTower
  where
    -- find the triple of numbers that sum to the left tower's height
    triples     = choose 3 boxes
    sumsToLeft  = filter (sum >>> (==left)) triples

    leftTower   = sortReverse $ head sumsToLeft
    rightTower  = sortReverse $ boxes \\ leftTower

    sortReverse = sortBy (flip compare)


-- This function is from: https://stackoverflow.com/a/14286085/229717
-- I swapped the parameter order for semantic clarity in doCase
choose :: Int -> [a] -> [[a]]
choose 0     _  = [[]]
choose _     [] =  []
choose k (x:xs) =  (x:) `fmap` (choose (k-1) xs) ++ choose k xs


{- Operations -}

towering :: String -> String
towering = parse >>> process
  where
    parse   = runWordScanner parseInput
    process = doCase >>> show


-- For running in the GHCi repl
try :: IO ()
try = towering <$> readFile "inputs/towering.input" >>= putStr

