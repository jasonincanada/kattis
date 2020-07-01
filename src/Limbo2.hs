{-  Limbo2 (difficulty 3.5) - https://open.kattis.com/problems/limbo2

    Revision:

    This has been refactored to reduce the number of named functions, in particular by
    calculating the details for a given Square or Rectangle all in one place (shapeProps)
    and returning a triple of values, instead of having six individual functions. There is
    still a lot of repetition of code and structure overall, so I'm pretty sure there's a
    more elegant way to write this, though whether that way will be more or less legible I
    don't know yet.

    I'm using the <|> operator for the first time. It provides a "short-circuit"
    functionality when used with the Maybe functor. This makes the doCase function clear
    in its intent: we try finding a Square first with the given coordinates, taking that
    result if it succeeds, or if it returns Nothing, trying then for a Rectangle.

    I'm still thinking of other ways to go about solving this problem. In particular I'd
    like to tell a story in doing so, about a mouse or bug on the grid making decisions
    based on things it notices about its environment, with the underlying implementation
    in categorical concepts, in particular sum types, product types, and monadic binding
    between the various considerations the mouse would have. My hope is that meaningfully
    naming the functions and arguments and letting the category theory concepts shimmer
    through will be an interesting new way to describe an algorithm and teach fundamental
    theory at the same time. But as of now it isn't clear to me how I would go about that.
    I suspect it's in an aggressive factoring out of all repetition and smartly naming the
    resulting code.

    Initially I stared for a while at my markup of the pattern described for the problem
    (see the image at ../notes/limbo2.jpg). I noticed a few properties but probably by no
    means all of them. (For example, the top-left block in each Square is a square, each
    Rectangle has a width twice that of its height). The growth pattern here was contrived
    by the author of this challenge for the purposes of the contest, and otherwise doesn't
    appear to me to be something that would be found in nature. So there may be
    interesting new things lurking a bit deeper in the structure here that could reward
    further thought.

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

