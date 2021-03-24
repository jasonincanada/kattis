{-  Above Average (difficulty 1.9) - https://open.kattis.com/problems/aboveaverage

    In this challenge we compute the percentage of numbers in a list that are greater than
    the average of the whole list. It's fairly straightforward, so we take advantage of
    the chance to practice some time traveling, in particular referencing a value that
    won't be computed until later

    For an introduction to this trick, see the following blog entry:

    https://blog.csongor.co.uk/time-travel-in-haskell-for-dummies/
-}

module AboveAverage (aboveaverage, try) where

import Data.Bifunctor   (bimap)
import Data.Traversable (mapAccumR)
import Text.Printf      (printf)


{- Types -}

type Grade   = Int
type Percent = Float
data Output  = Output [Percent]

instance Show Output where
  show (Output percentages) = unlines $ map format percentages
    where
      format :: Float -> String
      format = printf "%0.3f%%"


{- Parsing -}

{-  2
    5 50 50 70 80 100
    7 100 95 90 80 70 60 50
-}
parse :: String -> [[Grade]]
parse = map (map read . tail . words) . tail . lines



{- Methods -}

process :: [[Grade]] -> Output
process classes = Output percents
  where
    percents = map percent classes

    percent :: [Grade] -> Percent
    percent grades = fromIntegral over / fromIntegral total * 100
      where

        -- visit each grade in the list, accumulating the grade total (gt) and the number
        -- of grades (n) at each step. mapAccumR also maps over the structure as we go, so
        -- we swap the grade with whether or not it's above the average for the class. of
        -- course, we don't know the average yet since we're still gathering the two
        -- numbers we need to compute it. but we reference it anyway (1) and haskell
        -- remembers to finish the computation later once those two numbers are done being
        -- computed by the traversal
        --
        -- mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
        ((gt,n), overs) = mapAccumR check (0,0) grades

        check :: (Grade,Int) -> Grade -> ((Grade,Int), Bool)
        check (gt,n) grade = (accum, isAbove)
          where
            accum   = (gt+grade, n+1)
            isAbove = fromIntegral grade > average    -- (1)


        -- notice this value uses gt and n, but it's also referenced during the
        -- computation of gt and n
        average :: Percent
        average = fromIntegral gt / fromIntegral n


        -- second phase, we have our list of booleans specifying which grades were over
        -- the average, now calculate the percentage of Trues in the list. we could
        -- combine this and the above traversal into one
        (over, total) = foldr count (0,0) overs

        count :: Bool -> (Int,Int) -> (Int,Int)
        count isOver = bimap (if isOver then (+1) else id)
                             (+1)



{- Operations -}

aboveaverage :: String -> String
aboveaverage = show . process . parse

try :: IO ()
try = aboveaverage <$> readFile "test/inputs/aboveaverage-1.input" >>= putStrLn

