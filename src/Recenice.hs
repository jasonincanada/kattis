module Recenice (recenice, try) where

{-  Rečenice (difficulty 3.1) - https://open.kattis.com/problems/recenice -}

import Control.Arrow  ((>>>))
import Control.Monad.Writer
import Data.Function  ((&))
import Scanner


{- Types -}

type TestCase  = [String]
data Output    = Output [String]

instance Show Output where
  show (Output words) = unwords words


{- Parsing -}

{-  5
    this
    sentence
    has
    $
    letters
-}
parseInput :: Scanner TestCase
parseInput = numberOf str


{- Methods -}

doCase :: TestCase -> Output
doCase sentence = Output final
  where
    final   = map swap sentence

    swap w
      | w == "$"  = target
      | otherwise = w

    target  = head tries
    tries   = [ str | (n, str, len) <- numbers,
                      without + len == n ]

    -- number of letters without the goal word
    without = sentence & filter (/="$")
                       & map length
                       & sum

    -- all numbers from 1..999 with their descriptions, lengths of descriptions
    numbers = [ (i, str, length str) | i <- [1..999],
                                       let str = toString i ]


toString :: Int -> String
toString number = concat $ execWriter (convert number)

convert :: Int -> Writer [String] ()
convert number
  | number >= 100  = do let (q, r) = number `divMod` 100
                        tell [ones q ++ "hundred"]
                        convert r

  | number >= 20   = do let (q, r) = number `divMod` 10
                        tell [tens q]
                        convert r

  | number >= 10   = tell [teens number]
  | number == 0    = return ()
  | otherwise      = tell [ones number]
  where
    ones 1 = "one"
    ones 2 = "two"
    ones 3 = "three"
    ones 4 = "four"
    ones 5 = "five"
    ones 6 = "six"
    ones 7 = "seven"
    ones 8 = "eight"
    ones 9 = "nine"

    teens 10 = "ten"
    teens 11 = "eleven"
    teens 12 = "twelve"
    teens 13 = "thirteen"
    teens 14 = "fourteen"
    teens 15 = "fifteen"
    teens 16 = "sixteen"
    teens 17 = "seventeen"
    teens 18 = "eighteen"
    teens 19 = "nineteen"

    tens 2 = "twenty"
    tens 3 = "thirty"
    tens 4 = "forty"
    tens 5 = "fifty"
    tens 6 = "sixty"
    tens 7 = "seventy"
    tens 8 = "eighty"
    tens 9 = "ninety"


{- Operations -}

recenice :: String -> String
recenice = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = recenice <$> readFile "test/inputs/recenice-1.input" >>= putStr

