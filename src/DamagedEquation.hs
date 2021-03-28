{-  Damaged Equation (difficulty 2.8) - https://open.kattis.com/problems/damagedequation -}

module DamagedEquation (damagedequation, try) where

import Data.Maybe  (mapMaybe)
import Text.Printf (printf)


{- Types -}

type TestCase = [Int]
data Output   = Output [String]

instance Show Output where
  show (Output []       ) = "problems ahead"
  show (Output equations) = unlines equations


{- Parsing -}

{-  1 2 3 4  -}
parse :: String -> TestCase
parse = map read . words . head . lines



{- Methods -}

-- represent an operation along with its character symbol
data Oper = Oper Char (Int->Int->Int)


process :: TestCase -> Output
process (a:b:c:d:_) = Output equations
  where
    equations :: [String]
    equations = mapMaybe eval pairs

    -- mapMaybe :: (a -> Maybe b) -> [a] -> [b]

    eval :: (Oper, Oper) -> Maybe String
    eval (Oper leftChar  leftOp,
          Oper rightChar rightOp)

      -- catch division by zero
      | leftChar  == '/' && b == 0     = Nothing
      | rightChar == '/' && d == 0     = Nothing

      -- the equals case
      | a `leftOp` b == c `rightOp` d  = Just $ printf "%d %c %d = %d %c %d"
                                                       a leftChar  b
                                                       c rightChar d

      | otherwise = Nothing


    -- all pairs of operations
    pairs :: [ (Oper, Oper) ]
    pairs = [ (left, right) | left  <- opers,
                              right <- opers ]
    
    opers :: [Oper]
    opers = [ Oper '*' (*)
            , Oper '+' (+)
            , Oper '-' (-)
            , Oper '/' div  -- integer division (discard the remainder)
            ]




{- Operations -}

damagedequation :: String -> String
damagedequation = show . process . parse

try :: IO ()
try = damagedequation <$> readFile "test/inputs/damagedequation-1.input" >>= putStrLn

