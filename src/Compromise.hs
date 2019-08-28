module Compromise (compromise, try) where

{-  Compromise (difficulty 2.1) - https://open.kattis.com/problems/compromise -}

import Control.Arrow ((>>>))
import Control.Monad (replicateM)
import Data.List     (transpose)
import Scanner


{- Types -}

type Beliefs    = String          -- "1001010"
type TestCase   = [Beliefs]

-- The output object will be different for each challenge.  It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Output Beliefs

instance Show Output where
  show (Output belief) = belief


{- Parsing -}

parseInput :: Scanner [TestCase]
parseInput = str >> many parseCase

parseCase :: Scanner TestCase
parseCase = do
  count   <- runWordScanner int <$> str
  beliefs <- replicateM count str

  return beliefs

  -- equivalent but less readable:
  --
  -- parseCase = runWordScanner int <$> str >>= flip replicateM str


{- Logic -}

doCase :: TestCase -> Output
doCase beliefs = Output best
  where
    best    = determine sums
    sums    = map sum pm
    pm      = map (map plusMinus) $ transpose beliefs

    -- count a 0 as a -1 so we can simply sum the list
    plusMinus :: Char -> Int
    plusMinus '1' =  1
    plusMinus '0' = -1

    -- if the resulting sum is positive, agree on a 1 for this issue
    determine :: [Int] -> String
    determine []  = ""
    determine (s:ss)
      | s > 0     = '1' : determine ss
      | otherwise = '0' : determine ss


{- Operations -}

compromise :: String -> String
compromise = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = compromise <$> readFile "inputs/compromise.input" >>= putStr

