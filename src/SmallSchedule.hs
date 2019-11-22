module SmallSchedule (smallschedule, try) where

{-  SmallSchedule (difficulty 3.1) - https://open.kattis.com/problems/smallschedule

    This is a seemingly easy challenge that passes the sample inputs but fails on the main
    site's tests. The first 5 (of 52) tests pass but the 6th fails.  According to the
    stats page, 60% of attempts by participants have been wrong, so the thing I'm missing
    here is apparently common. I've not yet found the issue but I'm checking this in to
    have the incorrect revision on file.

-}

import Control.Arrow ((>>>))
import Scanner


{- Types -}

type TestCase = (Int, Int, Int, Int)


-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output Int

instance Show Output where
  show (Output minutes) = show minutes


{- Parsing -}

{-  2 4 3 6  -}
parseInput :: Scanner TestCase
parseInput = runWordScanner s <$> str
  where
    s = (,,,) <$> int  -- duration of the longer (Q) batches
              <*> int  -- machines we own
              <*> int  -- 1-second time slots purchased
              <*> int  -- Q-second time slots purchased



{- Logic -}

doCase :: TestCase -> Output
doCase (qdur, machines, ones, qs) = Output minutes
  where
    rows     = qs `div` machines
    leftover = qs `mod` machines
    gamma    = machines - leftover

    -- how many 1- minute slots we can run in parallel with the leftover
    -- Q-length processes, ie, they're "hidden" to the row-count, being overshadowed
    -- by the concurrently-running Qs
    hidden   = gamma*qdur

    minutes | leftover == 0  = (rows+0)*qdur + extra'
            | ones <= hidden = (rows+1)*qdur + 0
            | ones >  hidden = (rows+1)*qdur + extra
            where
              extra'= ceiling $ fromIntegral ones
                              / fromIntegral machines
              extra = ceiling $ fromIntegral (ones - hidden)
                              / fromIntegral machines



{- Operations -}

smallschedule :: String -> String
smallschedule = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = smallschedule <$> readFile "inputs/smallschedule-3.input" >>= putStr

