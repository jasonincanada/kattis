{-  Coffee Cup Combo - https://open.kattis.com/problems/coffeecupcombo  -}

module CoffeeCupCombo (coffeecupcombo) where

type TestCase = String
type Output   = Int

-- 10
-- 0100010100
parse :: String -> TestCase
parse input = head . tail . lines $ input

type Lectures = Int
type Holding  = Int

process :: TestCase -> Output
process machines = fst $ foldl f (0, 0) machines
  where
    f :: (Lectures, Holding) -> Char -> (Lectures, Holding)
    f   (lectures, _)      '1' = (lectures+1, 2)
    f x@(lectures, 0)       _  = x
    f   (lectures, holding) _  = (lectures+1, holding-1)

coffeecupcombo :: String -> String
coffeecupcombo = show . process . parse

