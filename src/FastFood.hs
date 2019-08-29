module FastFood (fastfood, try) where

{-  FastFood (difficulty 2.2) - https://open.kattis.com/problems/fastfood -}

import Control.Arrow ((>>>))
import Control.Monad (replicateM)
import qualified Data.Map as M
import Scanner


{- Types -}

type Sticker   =  Int
type CashValue =  Int
type Prize     = (CashValue, [Sticker]) -- cash value of this prize and the stickers required for it
type Pile      =  M.Map Sticker Int     -- the number of each sticker we have collected
type TestCase  = ([Prize], Pile)

-- The output object will be different for each challenge.  It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Output CashValue

instance Show Output where
  show (Output value) = show value


{- Parsing -}

parseInput :: Scanner [TestCase]
parseInput = str >> many parseCase

{-  2 10
    3 1 2 3 100
    4 4 5 6 7 200
    2 3 1 4 5 2 2 1 3 4
-}
parseCase :: Scanner TestCase
parseCase = do
  count    <- runWordScanner int          <$> str
  prizes   <- replicateM count parsePrize
  amounts  <- runWordScanner (many int)   <$> str

  return (prizes, pile amounts)

-- [20,50,80] -> [(1,20), (2,50), (3,80)]
pile :: [Int] -> Pile
pile amounts = M.fromList $ zip [1..] amounts


--  3 1 2 3 100
parsePrize :: Scanner Prize
parsePrize = runWordScanner p <$> str
  where
    p = do count    <- int
           stickers <- replicateM count int
           value    <- int
           return (value, stickers)


{- Logic -}

-- This challenge is relatively easy so we'll get a bit fancy with the computation.
-- Using the fact that (x,_) is a functor, we can fmap right within the tuple. Then
-- we combine the pair with an arbitrary function using uncurry
doCase :: TestCase -> Output
doCase (prizes, pile) = Output total
  where
    total  = sum $ map value prizes
    value  = fmap (findMin pile) >>> uncurry (*)

    -- fmap                :: (a -> b) -> (CashValue, a        ) -> (CashValue, b  )
    -- fmap (findMin pile) ::             (CashValue, [Sticker]) -> (CashValue, Int)

    -- uncurry :: (x -> y -> c) -> (x, y) -> c


    -- Look up how many of each of the required stickers we have, then find the minimum,
    -- which limits the number of this prize we can claim
    findMin :: Pile -> [Sticker] -> Int
    findMin pile = map (pile M.!) >>> minimum


{- Operations -}

fastfood :: String -> String
fastfood = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = fastfood <$> readFile "inputs/fastfood.input" >>= putStr

