module Lektira (lektira, try) where

{-  Lektira (difficulty 3.2) - https://open.kattis.com/problems/lektira

    This is the naive approach to this, which just cuts the candidate string all possible
    ways, sorts the whole list, then picks the first one.  I figured an optimization pass
    would be needed but it finishes the task in .01s and that's good enough.

-}

import Control.Arrow ((>>>))
import Data.List     (sort)
import Scanner


{- Types -}

type Fragment  = String
type TestCase  = String

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output String

instance Show Output where
  show (Output result) = result


{- Parsing -}

-- the input is a single string
parseInput :: Scanner TestCase
parseInput = str


{- Methods -}

doCase :: TestCase -> Output
doCase string = Output result
  where
    result = minimum (allStrings string)

    allStrings :: String -> [String]
    allStrings string = [ recombine $ cut string (i,j)
                                               | (i,j) <- indices (length string) ]


    -- all possible places to cut a string of length n into three non-blank substrings
    indices :: Int -> [ (Int,Int) ]
    indices n = [ (i,j) | i <- [1..n-2]
                        , j <- [2..n-1]
                        , j > i ]


    -- cut a string into three along the given indices
    cut :: String -> (Int,Int) -> (Fragment,Fragment,Fragment)
    cut s (i,j) = (left,mid,right)
      where
        left  = take i s                -- TODO: O(3n) traversal here could be O(n)
        mid   = take (j-i) $ drop i s
        right = drop j s


    -- reverse the components and concat them into the final string
    recombine :: (Fragment,Fragment,Fragment) -> String
    recombine (l,m,r) = concat [ reverse l
                               , reverse m
                               , reverse r ]



{- Operations -}

lektira :: String -> String
lektira = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = lektira <$> readFile "test/inputs/lektira-1.input" >>= putStr

