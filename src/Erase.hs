{-  Erase (difficulty 1.7) - https://open.kattis.com/problems/erase -}

module Erase (erase, try) where


{- Types -}

-- number of switches, list of bits at start, list at end
type TestCase  = (Int, [Char], [Char])

data Output    = Output Bool

instance Show Output where
  show (Output True)  = "Deletion succeeded"
  show (Output False) = "Deletion failed"


{- Parsing -}

{-  20
    0001100011001010
    0001000011000100
-}
parse :: String -> TestCase
parse input = (read switches, start, end)
  where
    (switches : start : end : _) = lines input



{- Methods -}

process :: TestCase -> Output
process (switches, start, end) = Output result
  where
    result :: Bool
    result = and compared

    compared :: [Bool]
    compared = zipWith comparer start end

    comparer :: Char -> Char -> Bool
    comparer = if even switches
               then (==)
               else (/=)



{- Operations -}

erase :: String -> String
erase = show . process . parse

try :: IO ()
try = erase <$> readFile "inputs/erase-1.input" >>= putStrLn

