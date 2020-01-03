{-# Language TupleSections #-}

module FlippingPatties (flippingpatties, try) where

{-  FlippingPatties (difficulty 3.2) - https://open.kattis.com/problems/flippingpatties

    A relatively basic challenge I picked out of order because it was new and I could
    sneak the first Haskell solution onto the board...

-}

import           Control.Arrow ((>>>))
import qualified Data.Array as A
import           Data.Function ((&))
import           Scanner


{- Types -}

type Duration = Int
type Time     = Int
type Due      = Time
type Order    = (Duration, Due)

type TestCase = [Order]


-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output Int

instance Show Output where
  show (Output cooks) = show cooks


{- Parsing -}

{-  3
    1 4
    2 5
    7 14
-}
parseInput :: Scanner TestCase
parseInput = numberOf order

order :: Scanner Order
order = parseLine $ (,) <$> int  -- duration per side
                        <*> int  -- target time to have the order ready



{- Logic -}

doCase :: TestCase -> Output
doCase orders = Output result
  where
    result  = ceiling $ fromIntegral busiest / 2  -- each cook has two hands
    busiest = maximum $ A.elems array             -- find the highest number of actions
                                                  -- taking place at the same time

    -- tally up the individual actions happening at each second by building an array
    -- from the list of action times.  accumArray handles the tallying by merging two
    -- simultaneous actions with a function, addition in this case
    array :: A.Array Time Int
    array = A.accumArray (+) 0 (0, 43200) actions
      where
        actions = concatMap times orders

        -- map an order to the list of times a cook performs an action
        times :: Order -> [(Time, Int)]
        times (d, t) = [ t - d - d  -- put the patty on the grill
                       , t - d      -- flip the patty
                       , t          -- serve it
                       ]
                         & map (,1) -- tuple them up for tallying

        -- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]



{- Operations -}

flippingpatties :: String -> String
flippingpatties = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = flippingpatties <$> readFile "inputs/flippingpatties-2.input" >>= putStr

