{-# Language DeriveFunctor #-}

module KnightSearch (knightsearch, try) where

{-  KnightSearch (difficulty 3.1) - https://open.kattis.com/problems/knightsearch

    This is the first of my three solutions to the Knight Search challenge. It uses an
    unfold/fold pattern with a coalgebra/algebra pair to first produce a tree of all valid
    knight traversals of the target sequence, then measure the tree's depth to determine
    if the whole path was traversed.

    This method is somewhat inefficient as it does some duplicate work that isn't needed
    to solve the problem; more on this in the comments in the second file.
    
    This solution has a 0.14s running time.

-}

import Control.Arrow ((>>>))
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

type Algebra   f a = f a -> a
type Coalgebra f a = a   -> f a

data TreeF a = NodeF Coord [a]    -- rose tree labeled with row/col coordinates
               deriving Functor

data Seed    = Seed Coord         -- coordinates of this placement
                    String        -- remainder of string to search for from here


doCase :: TestCase -> Output
doCase (side, board) = Output result
  where
    result   = length path `elem` searches
    searches = hylo coalgebra algebra <$> seeds board

    -- construct the list of starting seeds from the coordinates of the 'I's in the grid
    seeds :: Board -> [Seed]
    seeds = A.assocs >>> filter (snd >>> (=='I'))
                     >>> map fst
                     >>> map seed
      where
        seed coord = Seed coord $ tail path


    coalgebra :: Coalgebra TreeF Seed
    coalgebra (Seed coord []  ) = NodeF coord []
    coalgebra (Seed coord path) = NodeF coord subs
      where
        subs = [ Seed j rest | j <- jumps,

                               -- only jump if it's to the next letter in our path
                               board A.! j == next ]

        -- the next letter we're looking for and the remainder after that
        (next:rest) = path

        jumps :: [Coord]
        jumps = filter valid targets
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


    -- The coalgebra has already done all the thinking for us while unfolding the tree,
    -- so we are now just finding the tree's depth while folding it back up. We'll compare
    -- this to the length of our target path to make sure we've walked the whole thing
    algebra :: Algebra TreeF Int
    algebra (NodeF _ []) = 1
    algebra (NodeF _ cs) = 1 + maximum cs


hylo :: Functor f => Coalgebra f b -> Algebra f a -> b -> a
hylo coalg alg = coalg >>> fmap (hylo coalg alg) >>> alg



{- Operations -}

knightsearch :: String -> String
knightsearch = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = knightsearch <$> readFile "inputs/knightsearch-1.input" >>= putStr

