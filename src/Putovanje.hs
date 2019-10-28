module Putovanje (putovanje, try) where

{-  Putovanje (difficulty 2.7) - https://open.kattis.com/problems/putovanje

    Revision:

    This gives the correct answer on the 3 test inputs but I don't know yet if it works on
    the solution checker because Kattis at the moment isn't processing Haskell solutions
    (all but 6 languages are suddenly not available).  I'm checking in this revision
    because it appears to work with explicit recursion (the go function). The next
    revision will aim to have the recursion separated out into an algebra and a
    catamorphism.

-}

import Control.Arrow ((>>>))
import Data.List     (tails)
import Scanner


{- Types -}

type Capacity = Int
type Weight   = Int
type TestCase = (Capacity, [Weight])

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output   = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

{-  5 5
    3 1 2 1 1
-}
parseInput :: Scanner TestCase
parseInput = do
  capacity <- runWordScanner (str >> int) <$> str
  weights  <- map read . words            <$> str
  return (capacity, weights)


{- Logic -}

doCase :: TestCase -> Output
doCase (capacity, weights) = Output result
  where
    result = maximum tries
    tries  = [ count ws capacity | ws <- tails weights ]

    count :: [Weight] -> Capacity -> Int
    count ws cap = go ws cap 0
      where
        go _      0   n = n -- no capacity left
        go []     _   n = n -- no weights left
        go (w:ws) cap n
          | w <= cap    = go ws (cap-w) (n+1)
          | otherwise   = go ws  cap     n

    


{- Operations -}

putovanje :: String -> String
putovanje = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = putovanje <$> readFile "inputs/putovanje-3.input" >>= putStr

