{-# Language ViewPatterns #-}

{-  Majstor (difficulty 2.1) - https://open.kattis.com/problems/majstor

    I thought this one would have some sort of elegant solution but it looks more like
    something I'd give my cat for dinner
-}


module Majstor (majstor, try) where

import Data.List (transpose)


{- Types -}

data Symbol   = Rock | Paper | Scissors
type TestCase = ([Symbol], [[Symbol]])

type Score    = Int
data Output   = Output Score Score

instance Show Output where
  show (Output actual possible) = unlines [show actual,
                                           show possible]


{- Parsing -}

{-  5
    SSPPR
    2
    PPRRS
    RRSSP
-}
parse :: String -> TestCase
parse (lines -> (_:sven:_:friends)) = (sym <$> sven, map sym <$> friends)
  where
    sym :: Char -> Symbol
    sym 'R' = Rock
    sym 'P' = Paper
    sym 'S' = Scissors



{- Methods -}

type AccumO = (Score, Score )   -- actual, possible
type AccumI = (Score,[Score])   -- actual, score if sven had played [R,P,S]


process :: ([Symbol], [[Symbol]]) -> Output
process (sven, friends) = Output actual possible
  where
    (actual, possible) = foldr outer (0,0) zipped

    -- easier to fold over round-wise if we transpose columns to rows
    zipped :: [ (Symbol,[Symbol]) ]
    zipped = zip sven (transpose friends)


    outer :: (Symbol,[Symbol]) -> AccumO -> AccumO
    outer (sven,friends) (a,p) = (a+actual, p+possible)
      where
        (actual,tries) = foldr inner (0, [0,0,0]) friends
        possible       = maximum tries

        inner :: Symbol -> AccumI -> AccumI
        inner friend (actual, (r:p:s:_)) = (actual + score sven friend,

                                            [ r + score Rock     friend
                                            , p + score Paper    friend
                                            , s + score Scissors friend ])

        score :: Symbol -> Symbol -> Score
        score Rock     Rock      = 1
        score Rock     Paper     = 0
        score Rock     Scissors  = 2
        score Paper    Rock      = 2
        score Paper    Paper     = 1
        score Paper    Scissors  = 0
        score Scissors Rock      = 0
        score Scissors Paper     = 2
        score Scissors Scissors  = 1



{- Operations -}

majstor :: String -> String
majstor = show . process . parse

try :: IO ()
try = majstor <$> readFile "test/inputs/majstor-1.input" >>= putStrLn

