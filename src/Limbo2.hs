{-  Limbo2 (difficulty 3.7) - https://open.kattis.com/problems/limbo2

    Revision:

    Checking in the first version that works and passes the tests. Refactoring to follow


    notes: surprise that it is logBase 2, not logBase 4
           - probably will be better to start with the rectangle?
             - because it is the one that returns Maybe

-}

module Limbo2 (
  try,
  limbo2,

  which,trySquare,tryRect,doCase,Output(..)
) where


import Control.Applicative ((<|>))
import Control.Arrow       ((>>>))
import Data.Maybe          (fromJust)


{- Types -}

type TestCase  = [Int]      -- first two elements are the row/column

type Row       = Int
type Col       = Int
type Block     = Int
type Index     = Int


-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output Int
                 deriving Eq

instance Show Output where
  show (Output block) = show block


{- Parsing -}

-- 3
-- 0 0
-- 1 0
-- 2 4
parse :: String -> [TestCase]
parse = lines >>> drop 1 >>> map (words >>> map read)


{- Methods -}


doCase :: TestCase -> Output
doCase (row:column:_) = Output $ fromJust block
  where
    block  =     trySquare (which column) row column
             <|> tryRect   (which row   ) row column


-- map a column to the square it falls in, or a row to its rectangle; they turn out to be
-- the same function
which :: Int -> Index
which coord
  | coord == 0 = 0
  | otherwise  = 1 + (floor $ logBase 2 (fromIntegral coord))


-- if the nth square contains this row and column, return the block number at that coord
trySquare :: Index -> Row -> Col -> Maybe Block
trySquare index row col
  | row >= height  = Nothing
  | otherwise      = Just block
  where
    block                   = corner + (height * (col - left)) + row
    (corner, height, left)  = shapeProps index Square


tryRect :: Index -> Row -> Col -> Maybe Block
tryRect index row col
  | col >= width  = Nothing
  | otherwise     = Just block
  where
    block                 = corner + (width * (row - top)) + col
    (corner, width, top)  = shapeProps index Rectangle


data Shape   = Square | Rectangle
type Size    = Int
type Offset  = Int
type Corner  = Block


-- for the nth shape, calculate the number in its corner, its width|height, and location
shapeProps :: Index -> Shape -> (Corner, Size, Offset)

shapeProps n Square    | n == 0    = (0, 1, 0)
                       | otherwise = (4^(n-1)    , 2^(n-1), 2^(n-1))

shapeProps n Rectangle | n == 0    = error "shouldn't get here"
                       | otherwise = (4^n `div` 2, 2^n    , 2^(n-1))



{- Operations -}

limbo2 :: String -> String
limbo2 = parse >>> process >>> combine
  where
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = limbo2 <$> readFile "test/inputs/limbo2.input" >>= putStr

