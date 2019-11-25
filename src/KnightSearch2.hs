module KnightSearch2 (knightsearch, try) where

{-  KnightSearch (difficulty 3.1) - https://open.kattis.com/problems/knightsearch

    This is the second of my three solutions to the Knight Search challenge. The first one
    uses a tree as an intermediate data structure to unfold the structure of the valid
    walks, then folds it up to measure its depth.  This second solution is different in
    structure: it makes one pass through the string being walked, keeping track of all
    possible places the knight could be at each step.  Key to the performance of this
    solution is avoiding repetition: if two separate paths lead to the same intermediate
    square, only one traversal proceeds from there (this is the call to 'unique').  With
    the prior tree-based approach, each branch was a separate, independent walk with no
    efficiency gained from any overlapping of movements.

    This solution has a 0.03s running time compared to 0.14s for the first.

-}

import Control.Arrow ((>>>))
import Data.List     (group, sort)
import qualified Data.Array as A
import Scanner


{- Types -}

type Coord    = (Int, Int)          -- row, column
type Board    = A.Array Coord Char  -- row-major 2D array of coords/chars

type TestCase = (Int, Board)        -- side length, board


-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output   = Output Bool

instance Show Output where
  show (Output findable) | findable  = "YES"
                         | otherwise = "NO"

-- globals from the problem page
path   = "ICPCASIASG"


{- Parsing -}

{-  5
    IXIXXXXCXAXSXXPXXCSXAGXXX
-}
parseInput :: Scanner TestCase
parseInput = do side  <- int
                cells <- str
                return (side, makeBoard side cells)

makeBoard :: Int -> String -> Board
makeBoard side cells = A.listArray bounds cells
  where
    bounds = ((0,0), (side-1, side-1))



{- Methods -}

doCase :: TestCase -> Output
doCase (side, board) = Output result
  where
    result = go (starts board) (tail path)

    -- get the list of coordinates of the 'I's in the grid
    starts :: Board -> [Coord]
    starts = A.assocs >>> filter (snd >>> (=='I'))
                      >>> map fst

    go :: [Coord] -> [Char] -> Bool
    go []     _    = False             -- we've run out of places to jump
    go _      []   = True              -- we want to run out of letters
    go coords path = go coords' rest
      where
        coords' = unique [ j | c <- coords,
                               j <- jumps c,

                               -- only jump if it's to the next letter in our path
                               board A.! j == next ]

        -- the next letter we're looking for and the remainder after that
        (next:rest) = path

        jumps :: Coord -> [Coord]
        jumps coord = filter valid targets
          where
            valid (r,c) = and [ r >= 0  , c >= 0   ,
                                r < side, c < side ]

            targets     = jump coord <$> knightMoves 

            -- jumps for a knight relative to its current position
            knightMoves :: [Coord]
            knightMoves =  [
                                       (-2, -1), (-2, 1),
                             (-1, -2),                     (-1, 2),
                                              --N--
                             ( 1, -2),                     ( 1, 2),
                                       ( 2, -1), ( 2, 1)
                           ]

            jump :: Coord -> Coord -> Coord
            jump (r1,c1) (r2,c2) = (r1+r2, c1+c2)


unique :: Ord a => [a] -> [a]
unique  = sort >>> group >>> map head


{- Operations -}

knightsearch :: String -> String
knightsearch = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = knightsearch <$> readFile "inputs/knightsearch-1.input" >>= putStr

