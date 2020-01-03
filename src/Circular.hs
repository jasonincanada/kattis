module Circular (circular, try) where

{-  Circular (difficulty 2.7) - https://open.kattis.com/problems/circular 

    This passes the first 7 tests on Kattis but exceeds the 3-second time limit
    on the 8th
-}

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List     (group, sort, tails)
import Data.Monoid
import Scanner


{- Types -}

type Marker     = (Char, Int)
type TestCase   = [Marker]

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Output Int Int

instance Show Output where
  show (Output position count) = show position ++ " " ++ show count


{- Parsing -}

{-  9
    e1 e1 s1 e2 s1 s2 e42 e1 s1
-}
parseInput :: Scanner TestCase
parseInput = str >> parseLine (many marker)

marker :: Scanner Marker
marker = do
  (c:num) <- str
  return (c, read num)


{- Logic -}

type I   = Int  -- gene index
type Cut = Int  -- position into the strand to start the cut at

doCase :: TestCase -> Output
doCase markers = Output (cut+1) m
  where
    -- count the number of valid gene nestings for this cut of the dna strand
    count :: [Marker] -> Int
    count markers = foldMap test (seconds markers) & getSum
      where
        -- Is gene type i properly nested with this cut?
        test :: I -> Sum Int
        test i = check $ map fst $ filter (snd >>> (==i)) markers

        -- check if this sequence of start/end markers forms a properly nested structure
        -- according to the problem description (assumes the list is already filtered down
        -- to a specific i and therefore only consists of 's' and 'e')
        check :: [Char] -> Sum Int
        check ms = go ms 0
          where
            -- start of a new sequence
            go ('s':ms) c             = go ms (c+1)

            -- end of a sequence
            go ('e':ms) c | c == 0    = Sum 0
                          | otherwise = go ms (c-1)

            -- end of input, the counter is back to 0 if ms was properly nested
            go []       c | c == 0    = Sum 1
                          | otherwise = Sum 0
  

    prep :: [(Cut, [Marker])]
    prep = zip [0..] cuts
      where
        cuts    = take len $ map (take len) (tails doubled)
        len     = length markers
        doubled = markers ++ markers

    results :: [(Cut, Int)]
    results = map (fmap count) prep

    -- return the first cut whose count is the max of all the counts
    find :: [(Cut, Int)] -> (Cut, Int)
    find results = head $ filter (snd >>> (==maxM)) results
      where
        maxM = maximum $ map snd results

    (cut, m) = find results


-- Get the distinct second elements from a list of tuples
seconds :: Ord b => [(a,b)] -> [b] 
seconds = map snd >>> sort >>> group >>> map head


{- Operations -}

circular :: String -> String
circular = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = circular <$> readFile "inputs/circular-2.input" >>= putStr

