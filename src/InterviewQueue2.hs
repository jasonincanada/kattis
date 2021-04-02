{-# Language ViewPatterns #-}

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

     c) we're now using difference lists, which have constant-time append so we can avoid
        calling reverse all the time. i couldn't figure out how to pattern-match on them
        in the step function though, so I enabled ViewPatterns, and now call tl on each
        step to convert the difference list to a normal haskell list. then back to a diff
        list with fl before recursing on step. unfortunately, this round-trip seems to
        have caused the code to run slower overall, since it now fails on the fourth
        kattis test where it made it to the sixth test before. nonetheless, it feels like
        a step in the right direction

     d) this is actually doing two things per step: taking out values we want to print,
        and keeping values around for the next iteration--two really different operations

     e) idea: split the list into monotonically increasing segments and map over those
              segments somehow

        split:
        8 1 2 3 5 6 7

              -
              |
              |
              V

        8   1 2 3 5 6 7


        split:                                  step:
        3 6 2 3 2 2 2 1 5 6
              -
              |
              |
              V
        3 6   2 3   2 2 2   1 5 6               (3 2 2 1 5, 6 3 2 2 6)
        ^     ^
        init! |
        **not the 6, because it's greater than the number to the left in the same
          segment, and the first of the next segment is by definition less
              |
              |
              same reasoning as first segment, take the 2

                    ^
                    only the first of a repeated digit

                            ^ init takes both 1 and 5 here

-}

module InterviewQueue2 (interviewqueue2, try, step, Result(..)) where

import qualified Data.DList as DL
import Data.DList    (DList(..), empty, singleton)
import Data.Function ((&))



{- Types -}

type Value    = Int
type TestCase = [Value]

data Output   = Output Int (DL (DL Value))

instance Show Output where
  show (Output num values) = unlines $ show num : tl (DL.map (unwords . map show .
                                                      tl) values)


{- Aliases -}

type DL = DList

-- alias DList's element append operators to something more terse and fancy
(<++) :: a -> DL a -> DL a
(<++) = DL.cons

(++>) :: DL a -> a -> DL a
(++>) = DL.snoc

fl :: [a] -> DL a
fl = DL.fromList

tl :: DL a -> [a]
tl = DL.toList



{- Parsing -}

{-  7
    8 1 2 3 5 6 7
-}
parse :: String -> TestCase
parse = map read . words . (!! 1) . lines



{- Methods -}

data Accum = Accum Int (DL (DL Value))


process :: TestCase -> Output
process input = Output num vals
  where
    Accum num vals = go (fl input) (Accum 0 empty)
    
    -- go is the outer recursion, called every round until we have a round with
    -- no candidates leaving (always true for a single candidate left in line)

    go :: DL Value -> Accum -> Accum
    go (tl -> [x]) (Accum n vs) = Accum n (vs ++> singleton x)

    go values (Accum n vs)
      | null leave = Accum  n    (vs ++> remain)
      | otherwise  = Accum (n+1) (vs ++> leave ) & go remain

      where
        -- scan through the list of candidates, partitioning it into two lists,
        -- tracking who leaves and who remains in this round
        (leave, remain) = partition values


-- the result of a call to step: the first value from the prior step, if any,
-- and the values that left and remained in the queue
data Result a = Result { prior  :: Maybe Value
                       , leave  :: a
                       , remain :: a
                       } deriving (Eq, Show)

instance Functor Result where
  fmap f (Result p leave remain) = Result p (f leave) (f remain)


partition :: DL Value -> (DL Value, DL Value)
partition values = (leave, remain)
  where
    Result _ leave remain = step values (Result Nothing empty empty)



step :: DL Value -> Result (DL Value) -> Result (DL Value)

-- single-element lists
step (tl -> [v])   (Result Nothing leave remain)
                         = Result (Just v) leave remain

step (tl -> [v])   (Result (Just prior) leave remain)
  | v < prior            = Result (Just v) (leave ++> v) remain
  | otherwise            = Result (Just v) leave (remain ++> v)

-- lists with more than two elements, we consider only whether the current candidate
-- value v is leaving or remaining and let recursion on the tail consider the rest
step (tl -> (v:w:rest)) (Result Nothing leave remain)
  | v < w                = step (w <++ fl rest) (Result (Just v) (leave ++> v) remain)
  | otherwise            = step (w <++ fl rest) (Result (Just v) leave (remain ++> v))

step (tl -> (v:w:rest)) (Result (Just prior) leave remain)
  | v < w || v < prior   = step (w <++ fl rest) (Result (Just v) (leave ++> v) remain)
  | otherwise            = step (w <++ fl rest) (Result (Just v) leave (remain ++> v))



{- Operations -}

interviewqueue2 :: String -> String
interviewqueue2 = show . process . parse

try :: IO ()
try = interviewqueue2 <$> readFile "test/inputs/interviewqueue-1.input" >>= putStrLn

