module Commercials (commercials, try) where

{-  Commercials (difficulty 1.9) - https://open.kattis.com/problems/commercials

  This challenge requires us to find the optimal segment of all segments of a list
  according to a scoring rubric. We use a known trick to turn a cubic-time algorithm into
  a single-pass linear-time algorithm (see [Emoto] p. 568)

  This is close to the Maximum Segment Sum problem except there's a fixed cost of d
  dollars per time slot, so our symbol set must encompass not only the total listeners but
  the number of time slots listened to. The semiring tracks these separately and its
  operations combine them as simple sums


  References:
  
  [Emoto] https://www.jstage.jst.go.jp/article/imt/7/2/7_567/_article

-}


{- Types -}

-- cost of each timeslot, listeners per timeslot
type TestCase  = (Int, [Int])

data Output    = Output Int

instance Show Output where
  show (Output optimal) = show optimal


{- Parsing -}

{-  6 20
    18 35 6 80 15 21
-}
parse :: String -> TestCase
parse input = (d, nums)
  where
    d = read . head . drop 1 . words . head . lines $ input
    nums = map read . words . head . drop 1 . lines $ input



{- Logic -}

-- the symbol set for our semiring
data Sym = NegInf
         | Elem Int Int     -- count, sum


doCase :: TestCase -> Output
doCase (d, nums) = Output highest
  where
    highest  = n - (c*d)
    Elem c n = mits max mult f nums

    -- our semiring's "add" operation
    max :: Sym -> Sym -> Sym
    max NegInf x = x
    max x NegInf = x

    max a@(Elem c1 n1) b@(Elem c2 n2)
      = if n1-(c1*d) > n2-(c2*d)
        then a
        else b

    -- our semiring's "multiplication" operation
    mult :: Sym -> Sym -> Sym
    mult NegInf _ = NegInf
    mult _ NegInf = NegInf

    mult a@(Elem c1 n1) b@(Elem c2 n2)
      = Elem (c1+c2) (n1+n2)

    -- prepare an individual element by casting it into the semiring's set of symbols
    f n = Elem 1 n


-- a linear-time reducer that finds the optimal segment of all possible segments of a
-- list, constructed from the semiring operations of the equivalent cubic-time reducer
--
-- this is transcribed directly from [Emoto] p. 568.  i named it mits because of the
-- letters used to label the tuples
mits :: (b -> b -> b)   -- (+)
     -> (b -> b -> b)   -- (x)
     -> (a -> b)        -- preparer
     -> [a]
     -> b
mits add mult f = ex . foldr1 red . map g
  where
    red (m1, i1, t1, s1) (m2, i2, t2, s2)
      = (m1 `add` (t1 `mult` i2) `add` m2,
         i1 `add` (s1 `mult` i2),
         (t1 `mult` s2) `add` t2,
         s1 `mult` s2)

    g a = (f a, f a, f a, f a)
    ex (m, _, _, _) = m



{- Operations -}

commercials :: String -> String
commercials = show . process . parse
  where
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = commercials <$> readFile "inputs/commercials-1.input" >>= putStr

