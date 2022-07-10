{-  Skolvagen - https://open.kattis.com/problems/skolvagen  -}

module Skolvagen (skolvagen) where

import qualified Data.Array as A

data Crossing  = North | South | Both
data Side      = Top | Bottom
                 deriving (A.Ix, Eq, Ord)

data TestCase  = TestCase [Crossing]
type Output    = Int

parse :: String -> TestCase
parse input = TestCase . map crossing . head . lines $ input
  where
    crossing 'N' = North
    crossing 'S' = South
    crossing 'B' = Both


type Index = Int   -- index into the input list of crossings

process :: TestCase -> Output
process (TestCase crossings) = array A.! (n,Top)
  where
    n = length crossings

    array :: A.Array (Index, Side) Int
    array = A.listArray bounds [ f i side | (i,side) <- A.range bounds ]
      where
        bounds = ((0, Top),
                  (n, Bottom))

    -- calculate minimum number of crossings to get from the far left to this crossing/this side
    f :: Index -> Side -> Int

    -- base cases
    f 0 Top      = 0
    f 0 Bottom   = 1    -- crossing south from the starting position

    -- recursive cases
    f i Top      = min (rec (i-1) Top     + crossingCost i Top     + 0)
                       (rec (i-1) Bottom  + crossingCost i Bottom  + 1)

    f i Bottom   = min (rec (i-1) Bottom  + crossingCost i Bottom  + 0)
                       (rec (i-1) Top     + crossingCost i Top     + 1)

    -- the recursive step refers back into the dp array
    rec :: Index -> Side -> Int
    rec i s = array A.! (i,s)

    -- if we're crossing eastwards on the north side, it costs 1 crossing only if
    -- the intersection type is North or Both (free if there's no street to cross)
    crossingCost :: Int -> Side -> Int
    crossingCost i Top    = case crossings !! (i-1) of
                              North -> 1
                              South -> 0
                              Both  -> 1

    crossingCost i Bottom = case crossings !! (i-1) of
                              North -> 0
                              South -> 1
                              Both  -> 1

skolvagen :: String -> String
skolvagen = show . process . parse

