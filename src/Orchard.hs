module Orchard (orchard, try) where

{-  Orchard (difficulty 2.7) - https://open.kattis.com/problems/orchard

    This is a dynamic programming solution to the First Orchard problem using
    lazy array evaluation.  It quickly and exactly computes the probability of
    winning the First Orchard game given a particular starting state.
    
    It's a relatively straightforward problem with one complication: there's a
    chance we will roll a color that we have no more fruit for, in which case
    nothing happens and we roll again.  This almost complicates the computation
    by making us consider infinite series, but we can avoid this by simply
    ignoring "dud rolls" and considering the probability of hitting a non-dud
    roll.  This is explained in the stackexchange answer below, but I had some
    trouble understanding the concept from the text alone.  Fortunately in a
    recent video, Mike Lawler goes over this same concept while analysing
    probabilities in the casino game craps, and this made the idea click for me.


    References:

    Odds of winning Orchard Game?
    https://math.stackexchange.com/a/1957615

    Family Math 968c - Mike Lawler and sons:
    https://www.youtube.com/watch?v=umWslE_TRcw&t=121

-}

import           Control.Arrow ((>>>))
import qualified Data.Array as A
import           Data.List     (sort)
import           Scanner


{- Types -}

type Fruit     = Int
type Steps     = Int
type Prob      = Double
type GameState = ([Fruit], Steps)

type TestCase  = GameState

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output   = Output Prob

instance Show Output where
  show (Output prob) = show prob


{- Parsing -}

{-  1 1 0 0 3  -}
parseInput :: Scanner TestCase
parseInput = parseLine $ do fruits <- four int
                            steps  <- int
                            return (fruits, steps)


{- Logic -}

doCase :: TestCase -> Output
doCase (fruits, steps) = Output result
  where
    result = next (fruits, steps)

    -- Construct our (evaluated-on-demand) array of all possible states to
    -- avoid recomputing the same one twice
    array :: A.Array (Int,Int,Int,Int,Int) Prob
    array  = A.listArray bounds [ prob ([r,g,b,y], s) | (r,g,b,y,s) <- A.range bounds ]
      where
        [r,g,b,y] = sort fruits
        bounds    = ((0,0,0,0,0), (r,g,b,y,steps))


    -- Our main calculation function. It recurses, not back into itself directly,
    -- but by referencing an element in the above array, which will then call prob.
    -- There are only 4 fruit types so we list them out individually to illuminate
    -- the pattern, though if there were n types we'd want to generalize this.
    -- If we roll a 'basket' we get to choose which fruit we pick. We follow the
    -- strategy of picking from the most plentiful fruit type. This is always the
    -- last element in the list since we make sure we always have a sorted list
    prob :: GameState -> Prob

    prob (_        , 0) =   0.0  -- raven has no steps left - raven wins!
    prob ([0,0,0,0], _) =   1.0  -- no fruit left to pick   - players win!

    prob ([0,0,0,y], s) =   1/3 * next ([0  ,0  ,0  ,y-1], s  ) -- yellow
                          + 1/3 * next ([0  ,0  ,0  ,y-1], s  ) -- basket
                          + 1/3 * next ([0  ,0  ,0  ,y  ], s-1) -- raven

    prob ([0,0,b,y], s) =   1/4 * next ([0  ,0  ,b  ,y-1], s  ) -- yellow
                          + 1/4 * next ([0  ,0  ,b-1,y  ], s  ) -- blue
                          + 1/4 * next ([0  ,0  ,b  ,y-1], s  ) -- basket
                          + 1/4 * next ([0  ,0  ,b  ,y  ], s-1) -- raven

    prob ([0,g,b,y], s) =   1/5 * next ([0  ,g  ,b  ,y-1], s  ) -- yellow
                          + 1/5 * next ([0  ,g  ,b-1,y  ], s  ) -- blue
                          + 1/5 * next ([0  ,g-1,b  ,y  ], s  ) -- green
                          + 1/5 * next ([0  ,g  ,b  ,y-1], s  ) -- basket
                          + 1/5 * next ([0  ,g  ,b  ,y  ], s-1) -- raven

    prob ([r,g,b,y], s) =   1/6 * next ([r  ,g  ,b  ,y-1], s  ) -- yellow
                          + 1/6 * next ([r  ,g  ,b-1,y  ], s  ) -- blue
                          + 1/6 * next ([r  ,g-1,b  ,y  ], s  ) -- green
                          + 1/6 * next ([r-1,g  ,b  ,y  ], s  ) -- red
                          + 1/6 * next ([r  ,g  ,b  ,y-1], s  ) -- basket
                          + 1/6 * next ([r  ,g  ,b  ,y  ], s-1) -- raven


    -- Helper function that first sorts the fruits before indexing into the array.
    -- We can sort at whim without loss of generality because each new roll has an
    -- equal chance of picking each color
    next (fruits, steps) = let [r,g,b,y] = sort fruits
                           in  array A.! (r,g,b,y,steps)



{- Operations -}

orchard :: String -> String
orchard = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = orchard <$> readFile "inputs/orchard-2.input" >>= putStr

