{-  Interview Queue (difficulty 7.2) - https://open.kattis.com/problems/interviewqueue

    This passes the first five tests but exceeds the 4s time limit on the sixth :(

-}

module InterviewQueue (interviewqueue, try, step, Result(..)) where

import Data.Function ((&))


{- Types -}

type Value    = Int
type TestCase = [Value]

data Output   = Output Int [[Value]]

instance Show Output where
  show (Output num values) = unlines $ show num : map (unwords . map show) values


{- Parsing -}

{-  7
    8 1 2 3 5 6 7
-}
parse :: String -> TestCase
parse = map read . words . (!! 1) . lines



{- Methods -}

data Accum = Accum Int [[Value]]


process :: TestCase -> Output
process input = Output num (reverse vals)
  where
    Accum num vals = go input (Accum 0 [])
    
    -- go is the outer recursion, called every round until we have a round with
    -- no candidates leaving (always true for a single candidate left in line)

    go :: [Value] -> Accum -> Accum
    go [x] (Accum n vs) = Accum n ([x] : vs)

    go values (Accum n vs)
      | null leave = Accum  n    (remain : vs)
      | otherwise  = Accum (n+1) (leave  : vs) & go remain
      where
        Result leave remain _ = step values


-- the result of a call to step: the values removed, values kept,
-- and whether the first candidate in line was kept
data Result = Result { leave     :: [Value]
                     , remain    :: [Value]
                     , keepFirst :: Bool
                     } deriving (Eq, Show)


-- step is the inner recursion, called n-1 times for a list of length n. it recurses right
-- away on the tail of the list, then combines the results with the current step,
-- depending on the local comparison between the first and second elements in the list

step :: [Value] -> Result 
step [v,w]
  | v < w     = Result [v] [w]   False
  | v > w     = Result [w] [v]   True
  | otherwise = Result []  [w,v] True

step (v:w:rest)
  | v < w     = Result (v : leave)
                       remain
                       False

  | v > w     = Result (if kept_w then w : leave       else leave     )
                       (if kept_w then v : tail remain else v : remain)
                       True

  | otherwise = Result leave
                       (v : remain)
                       True
  where
    Result leave remain kept_w = step (w:rest)



{- Operations -}

interviewqueue :: String -> String
interviewqueue = show . process . parse

try :: IO ()
try = interviewqueue <$> readFile "test/inputs/interviewqueue-1.input" >>= putStrLn

