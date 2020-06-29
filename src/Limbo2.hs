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

  which,trySquare,tryRect,doCase,Output(..),rectCorner,rectTop
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


trySquare :: Index -> Row -> Col -> Maybe Block
trySquare square row col
  | row >= height  = Nothing
  | otherwise      = Just block
  where
    block  = corner + (height * (col - left)) + row

    corner = squareCorner square
    height = squareHeight square
    left   = squareLeft   square


tryRect :: Index -> Row -> Col -> Maybe Block
tryRect rect row col
  | col >= width  = Nothing
  | otherwise     = Just block
  where
    block  = corner + (width * (row - top)) + col

    corner = rectCorner rect
    width  = rectWidth  rect
    top    = rectTop    rect


squareCorner :: Index -> Block
squareCorner 0 = 0
squareCorner n = 4^(n-1)

squareHeight :: Index -> Row
squareHeight 0 = 1
squareHeight n = 2^(n-1)

squareLeft :: Index -> Col
squareLeft 0 = 0
squareLeft n = 2^(n-1)

rectCorner :: Index -> Block
rectCorner 0 = error "should never get here"
rectCorner n = 4^n `div` 2

rectWidth :: Index -> Col
rectWidth 0 = error "should never get here"
rectWidth n = 2^n

rectTop :: Index -> Row
rectTop 0 = error "should never get here"
rectTop n = 2^(n-1)


which :: Int -> Int
which coord
  | coord == 0 = 0
  | otherwise  = 1 + (floor $ logBase 2 (fromIntegral coord))



{- Operations -}

limbo2 :: String -> String
limbo2 = parse >>> process >>> combine
  where
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = limbo2 <$> readFile "test/inputs/limbo2.input" >>= putStr

