{-# Language DeriveFunctor #-}
{-# Language ViewPatterns  #-}

module MarblesTree (marblestree, try) where

{-  MarblesTree (difficulty 3.0) - https://open.kattis.com/problems/marblestree

    This is my first non-trivial algebra.  It attempts to efficiently fold up the marble
    tree by tracking only the summary data needed, throwing away the subtrees' structure
    as it goes.  It works from the leaf nodes up, at each node merging the results of the
    subtrees into an accumulator (FoldData):
    
      1) the accumulating cost (the number of moves of the marbles), and
      2) one of:
         a) the surplus number of marbles we're carrying upwards, or
         b) the distances to zeros we still have to fill in the subtrees we've left behind

    It took many days of thinking to figure out this algebra, so I was excited when it
    finally ran accurately on the test inputs.  However, I must have made a wrong
    assumption somewhere since it only passes the first test on Kattis and fails the
    second :(  Of course, Kattis doesn't show you anything about what went wrong, just
    that your answer isn't correct!  I'm pretty sure the code represents my assumptions
    accurately, so I may have missed something while reasoning about the algebra.

-}

import           Control.Arrow ((>>>))
import           Data.Function ((&))
import qualified Data.Map as M
import           Data.Monoid   (Sum(..))
import           Scanner


{- Types -}

data TreeF a    = NodeF Int [a]    -- rose tree labeled with marble count
                  deriving Functor

newtype Fix f   = Fix { unFix :: f (Fix f) }
type MarbleTree = Fix TreeF

type Label      = Int
type TestCase   = M.Map Label (Int, [Label])


-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Output Int

instance Show Output where
  show (Output moves) = show moves


{- Parsing -}

{-  9
    1 2 3 2 3 4
    2 1 0
    3 0 2 5 6
    4 1 3 7 8 9
    ...
-}
parseInput :: Scanner [TestCase]
parseInput = init <$> many parseCase

parseCase :: Scanner TestCase
parseCase = toMap <$> numberOf node
  where
    node :: Scanner (Label, (Int, [Label]))
    node = runWordScanner n <$> str

    -- 3 0 2 5 6  -> (3, (0, [5,6]))
    n = do label     <- int
           marbles   <- int
           subLabels <- numberOf int

           return (label, (marbles, subLabels))

    toMap = M.fromList



{- Logic -}

doCase :: TestCase -> Output
doCase dict = Output result
  where
    result = hylo (coalgebra dict) algebra seed  & fst
    seed   = (1, dict M.! 1  & snd)


-- Coalgebra
type Coalgebra f a = a -> f a

-- one step of the unfolding of the map into its tree representation
coalgebra :: TestCase -> Coalgebra TreeF (Label, [Label])
coalgebra dict (label, labels) = NodeF marbles subs
  where
    marbles = dict M.! label  & fst
    subs    = [ (l, dict M.! l  & snd) | l <- labels ]


-- Algebra

data TreeSummary = Surplus  Int       -- either a surplus of n (possibly 0) marbles or
                 | Deficit [Int]      -- a list of number of steps to known zero nodes

type FoldData    = (Int, TreeSummary) -- (total moves so far, subtrees summarized)

type Algebra f a = f a -> a

algebra :: Algebra TreeF FoldData
algebra (NodeF marbles []    )               -- leaf node base cases
  | marbles == 0  = (0, Deficit $ [0]      )
  | otherwise     = (0, Surplus $ marbles-1)

algebra (NodeF marbles subtrees) = foldData  -- the main folding action
  where

    -- summarize the subtrees by monoidally merging them
    (getSum -> oldCost,
     getSum -> surplus,
               zeroPaths) = foldMap f subtrees
      where
        f :: FoldData -> (Sum Int, Sum Int, [Int])
        f (cost, Deficit ds) = (Sum cost, mempty, ds    )
        f (cost, Surplus s ) = (Sum cost, Sum s , mempty)


    -- the cost of moving the surplus up to the current node is one jump per marble
    surplusCost = 1 * surplus

    -- new number of marbles at this node before we start sending them down to the zeros
    marbles'    = marbles + surplus

    foldData    = move marbles' zeroPaths

    move :: Int -> [Int] -> FoldData
    move marbles (map (+1) -> paths)  -- paths to zeros are all now 1 jump longer.
                                      -- do the mapping on the way in with ViewPatterns

      -- send all the marbles down and account for the new zero at this node
      | marbles <= zeros  = (tally + cost marbles, Deficit $ 0 : remaining       )

      -- send as many as needed down and record the leftover as surplus at this node
      | marbles >  zeros  = (tally + cost zeros  , Surplus $ marbles - zeros - 1 )

      where
        -- the cost so far plus the cost of moving the surplus marbles
        tally = oldCost + surplusCost

        -- number of paths downwards to known zeros
        zeros = length paths

        -- the cost of traveling the first n paths
        cost n = sum (take n paths) 

        -- the paths to the remaining zeros
        remaining = drop marbles paths



hylo :: Functor f => Coalgebra f b -> Algebra f a -> b -> a
hylo coalg alg = coalg >>> fmap (hylo coalg alg) >>> alg


{- Operations -}

marblestree :: String -> String
marblestree = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = marblestree <$> readFile "inputs/marblestree.input" >>= putStr

{-
    λ> MarblesTree.try
    7
    14
    20
-}

