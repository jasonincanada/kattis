module ThreePowers (threepowers, try) where

{-  ThreePowers (difficulty 2.5) - https://open.kattis.com/problems/threepowers

-}

import Control.Arrow  ((>>>))
import Data.Bool      (bool)
import Data.List      (intercalate)
import Scanner


{- Types -}

type TestCase  = Integer

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output [Integer]

instance Show Output where
  show (Output []  ) = "{ }"
  show (Output ints) = "{ " ++ list ++ " }"
    where
      list = intercalate ", " (map show ints)


{- Parsing -}

-- Read all but the last of a list of integers
parseInput :: Scanner [TestCase]
parseInput = init <$> many bigint


{- Logic -}

doCase :: TestCase -> Output
doCase n = Output $ set 3 n

-- Get the nth subset of b^[0..] ordered by sum of elements
set :: Integer -> Integer -> [Integer]
set b n = foldMap try [0,1..last n]
  where
    last = fromIntegral >>> logBase 2 >>> floor

    try :: Integer -> [Integer]
    try i = bool [] [b^i] (ind == 1)
      where
        ind = (n-1) `div` (2^i) `mod` 2

-- 1-liner:
-- set b n = filter (>0) [ b^i * ((n-1) `div` (2^i) `mod` 2) | i <- [0 .. (floor $ logBase 2 $ fromIntegral n)] ]


{- Operations -}

threepowers :: String -> String
threepowers = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = threepowers <$> readFile "inputs/threepowers.input" >>= putStr

