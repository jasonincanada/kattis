module JointAttack (jointattack, try) where

{-  JointAttack (difficulty 2.5) - https://open.kattis.com/problems/jointattack

-}

import Control.Arrow  ((>>>))
import Scanner


{- Types -}

type Coefficient = Integer
type Fraction    = (Integer, Integer)

type TestCase    = [Coefficient]

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output      = Output Fraction

instance Show Output where
  show (Output (num, denom)) = show num ++ "/" ++ show denom


{- Parsing -}

-- Skip the first line and read all integers available on the second
parseInput :: Scanner TestCase
parseInput = runWordScanner (many bigint) <$> (str >> str)


{- Logic -}

doCase :: TestCase -> Output
doCase coeffs = Output $ continued coeffs

continued :: [Coefficient] -> Fraction
continued [a,b]    = add (a,1) (invert $ (b,1))
continued (a:rest) = add (a,1) (invert $ continued rest)

invert :: Fraction -> Fraction
invert (n,d) = (d,n)

add :: Fraction -> Fraction -> Fraction
add (a,b) (c,d) = reduce (a*d + c*b, b*d)

reduce :: Fraction -> Fraction
reduce (num, denom) = (num `div` g, denom `div` g)
  where
    g = gcd num denom


{- Operations -}

jointattack :: String -> String
jointattack = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = jointattack <$> readFile "inputs/jointattack.input" >>= putStr

