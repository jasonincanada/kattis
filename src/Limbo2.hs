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
  Rect(..)  , toRectangle,
  Square(..), toSquare,

  trySquare,tryRect,doCase,Output(..),rectCorner,rectTop
) where


import Control.Applicative ((<|>))
import Control.Arrow       ((>>>))
import Data.Maybe          (fromJust)


{- Types -}

type TestCase  = [Int]      -- first two elements are the row/column

type Row       = Int
type Col       = Int
type Block     = Int


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


-- A square (above the jagged blue line) is identified by its ordinal position from left
-- to right across the grid (0th, 1st, 2nd, 3rd etc)
data Square = Square Int 
              deriving (Eq, Show)

-- Same for rectangles but starting with 1st (the 0th square has the top-left corner)
data Rect   = Rect Int
              deriving (Eq, Show)


doCase :: TestCase -> Output
doCase (row:column:_) = Output $ fromJust block
  where
    block
      | row + column == 0  = Just 0
      | otherwise          =     trySquare square row column
                             <|> tryRect   rect   row column

    square = toSquare column
    rect   = fromJust $ toRectangle row


trySquare :: Square -> Row -> Col -> Maybe Block
trySquare square row col
  | row >= height  = Nothing
  | otherwise      = Just block
  where
    block  = corner + (height * (col - left)) + row

    corner = squareCorner square
    height = squareHeight square
    left   = squareLeft   square


tryRect :: Rect -> Row -> Col -> Maybe Block
tryRect rect row col
  | col >= width  = Nothing
  | otherwise     = Just block
  where
    block  = corner + (width * (row - top)) + col

    corner = rectCorner rect
    width  = rectWidth  rect
    top    = rectTop    rect


squareCorner :: Square -> Block
squareCorner (Square 0) = 0
squareCorner (Square n) = 4^(n-1)

squareHeight :: Square -> Row
squareHeight (Square 0) = 1
squareHeight (Square n) = 2^(n-1)

squareLeft :: Square -> Col
squareLeft (Square 0) = 0
squareLeft (Square n) = 2^(n-1)

rectCorner :: Rect -> Block
rectCorner (Rect n) = 4^n `div` 2

rectWidth :: Rect -> Col
rectWidth (Rect n) = 2^n

rectTop :: Rect -> Row
rectTop (Rect n) = 2^(n-1)


-- the column of the coord will always map to a unique square, but may not actually be in
-- it, that also depends on the row
toSquare :: Col -> Square
toSquare col
  | col == 0   = Square $ 0
  | otherwise  = Square $ 1 + (floor $ logBase 2 $ fromIntegral col)


-- the row maps to a unique rectangle if it's 1 or higher, otherwise none of them
toRectangle :: Row -> Maybe Rect
toRectangle row
  | row == 0   = Nothing
  | otherwise  = Just $ Rect $ 1 + (floor $ logBase 2 $ fromIntegral row)


{- Operations -}

limbo2 :: String -> String
limbo2 = parse >>> process >>> combine
  where
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = limbo2 <$> readFile "test/inputs/limbo2.input" >>= putStr

