module KnightSearch3 (knightsearch, try) where

{-  KnightSearch (difficulty 3.1) - https://open.kattis.com/problems/knightsearch

    This is my third solution for the KnightSearch challenge.  My goal for this one was to
    avoid explicit recursion (go calling go) by using foldl and passing a combining
    function (f) instead.  The accumulator in this case is a sum type (Accum) which gives
    some control flow to the algorithm even though it's probably a bit clunky to use a sum
    type as an accumulator.  This solution also uses de-duplication with the call to
    'unique', so it achieves the same performance as the second solution, at 0.03s.

    There is actually some more performance to gain in all three of these solutions since
    there are duplicate lookups/additions occurring.  The second time the knight lands on
    the same square, the code repeats the 8 lookups to the grid to figure out what's
    where.  The first lookups are unavoidable (I think?) but the second time landing on a
    square could use the memoized result from the prior call.  So performance could
    probably be improved even more from here.

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

data Accum   = Failed | Coords [Coord]

doCase :: TestCase -> Output
doCase (side, board) = Output result
  where
    result = case res' of
               Failed    -> False
               _         -> True

    res'   = foldl f (Coords $ starts board) (tail path)

    -- get the list of coordinates of the 'I's in the grid
    starts :: Board -> [Coord]
    starts = A.assocs >>> filter (snd >>> (=='I'))
                      >>> map fst

    f :: Accum -> Char -> Accum
    f Failed          _      = Failed
    f (Coords coords) letter
      | null coords'         = Failed
      | otherwise            = Coords coords'
      where
        coords' = unique [ j | c <- coords,
                               j <- jumps c,

                               -- only jump if it's to the next letter in our path
                               board A.! j == letter ]

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

