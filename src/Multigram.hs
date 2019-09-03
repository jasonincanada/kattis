module Multigram (multigram, try) where

{-  Multigram (difficulty 2.4) - https://open.kattis.com/problems/multigram -}

import Control.Arrow   ((>>>))
import Data.Bool       (bool)
import Data.List       (sort)
import Data.List.Split (chunksOf)
import Scanner


{- Types -}

type TestCase   = String

-- The output object will be different for each challenge.  It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Root String
                | NotAMultigram

instance Show Output where
  show (Root root)   = root
  show NotAMultigram = "-1"


{- Parsing -}

parseInput :: Scanner TestCase
parseInput = str


{- Logic -}

doCase :: TestCase -> Output
doCase word = output
  where
    output = case roots of
               (root:_) -> Root root
               []       -> NotAMultigram

    roots :: [String]
    roots = concat [ anagrams $ chunksOf i word | i <- divisors (length word) ]

    -- return the root of the list of words if they are all anagrams
    -- or the empty list if not
    anagrams :: [String] -> [String]
    anagrams (root:rest) = bool [] [root] allGood
      where
        allGood = all (sort >>> (==sorted)) rest
        sorted  = sort root

    -- from: https://stackoverflow.com/a/1480620/229717
    divisors :: Int -> [Int]
    divisors n = 1 : filter (rem n >>> (==0)) [2 .. n `div` 2]



{- Operations -}

multigram :: String -> String
multigram = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = multigram <$> readFile "inputs/multigram-3.input" >>= putStr

