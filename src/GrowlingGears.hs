{-# Language ViewPatterns #-}

module GrowlingGears (growlinggears, try) where

{-  GrowlingGears (difficulty 2.4) - https://open.kattis.com/problems/growlinggears

    This challenge requires us to find the maximum value of an inverted parabola. I didn't already
    know how to do this, so I googled it and used steps 3 and 4 from the following page:

    https://www.wikihow.com/Find-the-Maximum-or-Minimum-Value-of-a-Quadratic-Function-Easily
-}

import Data.Bifunctor (first)
import Control.Arrow  ((>>>))
import Control.Monad  (replicateM)
import Data.List      (sort)
import Scanner


{- Types -}

-- Coefficients (a, b, c) for a polynomial with degree <= 2 (ax^2 + bx + c)
type Gear      = (Int, Int, Int) 

type TestCase  = [Gear]

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output Int

instance Show Output where
  show (Output i) = show i


{- Parsing -}

parseInput :: Scanner [TestCase]
parseInput = str >> many parseGears

{-  2
    3 126 1400
    2 152 208
-}
parseGears :: Scanner TestCase
parseGears = do
  count <- int
  gears <- replicateM count parseGear

  return gears

--  3 126 1400
parseGear :: Scanner Gear
parseGear = do
  [a, b, c] <- runWordScanner (three int) <$> str
  return (negate a, b, c)


{- Logic -}

doCase :: TestCase -> Output
doCase gears = Output $ findMaxY withIndex
  where
    withIndex = zip gears [1..]

    findMaxY :: [(Gear, Int)] -> Int
    findMaxY = map (first yMax) >>> sort >>> last >>> snd
      where
        yMax :: Gear -> Float
        yMax gear = eval gear (xOfMax gear)


-- Evaluate the polynomial at a given x
eval :: Gear -> Float -> Float
eval (fromIntegral -> a,
      fromIntegral -> b,
      fromIntegral -> c) x = a*x^2 + b*x + c


-- Get the x coordinate of the maximum value of this parabola (we know it's inverted, i.e. a < 0)
xOfMax :: Gear -> Float
xOfMax (fromIntegral -> a,
        fromIntegral -> b,
        _                ) = -1 * b / (2 * a)


{- Operations -}

growlinggears :: String -> String
growlinggears = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = growlinggears <$> readFile "inputs/growlinggears.input" >>= putStr

