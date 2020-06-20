module Limbo1 (try, limbo1) where

{-  Limbo1 (difficulty 3.4) - https://open.kattis.com/problems/limbo1

    An easy one where we use a trick (described in the comments) to reduce the calculation
    to one pair of multiplication/division and a couple additions
    
    The runtime was 0.01s using the Scanner parsing code, so I removed it and did it with
    Haskell built-ins only, bringing the runtime to 0.00s

-}

import Control.Arrow ((>>>))


{- Types -}

type TestCase  = [Int]

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output Int

instance Show Output where
  show (Output result) = show result


{- Parsing -}

-- 3
-- 1 2
-- 2 3
-- 5 8
parse :: String -> [TestCase]
parse = lines >>> drop 1 >>> map (words >>> map read)


{- Methods -}

doCase :: TestCase -> Output
doCase (left:right:_) = Output result
  where

    -- here's the trick: do both left and right together as only one trip down
    -- the left hand side of the triangle
    result = both*(both+1) `div` 2 + 1

               -- then simply add right to compensate for having gone the wrong way
               + right

    both   = left+right


{- Operations -}

limbo1 :: String -> String
limbo1 = parse >>> process >>> combine
  where
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = limbo1 <$> readFile "test/inputs/limbo1.input" >>= putStr

