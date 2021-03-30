{-  Interview Queue 2 (difficulty 7.2) - https://open.kattis.com/problems/interviewqueue

    This iteration of the original InterviewQueue.hs uses a new inner recursion scheme in
    an attempt to improve runtime, but it's still exceeding the 4s time limit on the sixth
    kattis test, so we'll keep refactoring

    Revision Notes

     a) it turns out we don't need to consider the last two values as a pair, we can
        handle the singleton list by itself. this simplifies the code a lot

     b) step is a fold-like function, so we make it internal to a new function called
        partition. this lets us factor out the calls to reverse, since reversing the final
        accumulated strings shouldn't be the responsibility of the fold function. step
        could be indented to be local to partition but then the unit tests wouldn't see it

-}

module InterviewQueue2 (interviewqueue2, try, step, Result(..)) where

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
        -- scan through the list of candidates, partitioning it into two lists,
        -- tracking who leaves and who remains in this round
        (leave, remain) = partition values


-- the result of a call to step: the first value from the prior step, if any,
-- and the values that left and remained in the queue
data Result = Result { prior  :: Maybe Value
                     , leave  :: [Value]
                     , remain :: [Value]
                     } deriving (Eq, Show)


partition :: [Value] -> ([Value], [Value])
partition values = (reverse leave, reverse remain)
  where
    Result _ leave remain = step values (Result Nothing [] [])


step :: [Value] -> Result -> Result

-- single-element lists
step [v]   (Result Nothing leave remain)
                         = Result (Just v) leave remain

step [v]   (Result (Just prior) leave remain)
  | v < prior            = Result (Just v) (v:leave)    remain
  | otherwise            = Result (Just v)    leave  (v:remain)

-- lists with more than two elements, we consider only whether the current candidate
-- value v is leaving or remaining and let recursion on the tail consider the rest
step (v:w:rest) (Result Nothing leave remain)
  | v < w                = step (w:rest) (Result (Just v) (v:leave)    remain )
  | otherwise            = step (w:rest) (Result (Just v)    leave  (v:remain))

step (v:w:rest) (Result (Just prior) leave remain)
  | v < w || v < prior   = step (w:rest) (Result (Just v) (v:leave)    remain )
  | otherwise            = step (w:rest) (Result (Just v)    leave  (v:remain))



{- Operations -}

interviewqueue2 :: String -> String
interviewqueue2 = show . process . parse

try :: IO ()
try = interviewqueue2 <$> readFile "test/inputs/interviewqueue-1.input" >>= putStrLn

