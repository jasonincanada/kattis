module Uxuhul (ltop, ptol, pile, turn, uxuhul, try, Stone(..)) where

{-  Uxuhul (difficulty 3.2) - https://open.kattis.com/problems/uxuhulvoting

    I couldn't get this one to work on inputs with more than one voter and I'm not sure
    where I've gone wrong. This is a dynamic programming solution with my trusty
    lazily-evaluated array. I think I've reasoned through the problem properly but
    evidently I haven't, or I've implemented my assumptions improperly.

    I'm checking it in anyway but it's currently not giving the correct response to the
    sample tests. It's somewhat clunky code, I was looking forward to refactoring it into
    a less dense tangle, but it's not correct anyway so I'm moving on for now

-}

import Control.Arrow ((>>>))
import Data.Bool     (bool)
import Data.List     (minimumBy)
import Data.Ord      (comparing)
import qualified Data.Array as A

import Scanner


{- Types -}

type Prefs     = [Int]
type TestCase  = [Prefs]

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output [Int]

instance Show Output where
  show (Output outcome) = map (\n -> bool 'N' 'Y' $ n == 1) outcome


{- Parsing -}

parseInput :: Scanner [TestCase]
parseInput = numberOf vote
  where
    vote :: Scanner TestCase
    vote = numberOf prefs

    prefs :: Scanner Prefs
    prefs = parseLine (many int)


{- Methods -}


type Voter       =  Int -- 1..n
type Preference  =  Int -- 0..7, rank of a given pile, 0th is the best


-- 1..3
data Stone       = Stone Int
                   deriving Show

-- some smart constructors to make sure we have sane inputs
stone :: Int -> Stone
stone s | s == 0    = error "Stone is 1-based"
        | s >  3    = error "only 3 stones"
        | otherwise = Stone s


-- 1..8, the various configurations the stones can be in
data Pile        = Pile Int
                   deriving Show

pile :: Int -> Pile
pile p | p == 0    = error "Pile is 1-based"
       | p >  8    = error "only 8 possible piles with 3 stones"
       | otherwise = Pile p


-- a partial indexOf that will crash on empty list or element not found in list
indexOf :: Eq a => a -> [a] -> Int
indexOf x = go 0
  where
    go i (a:as)
      | x == a    = i
      | otherwise = go (i+1) as

    go _ []       = error "not found in list"
  

doCase :: TestCase -> Output
doCase prefs = Output result
  where
    result = ptol final
    voters = length prefs

    final  = last $ go $ ltop [-1,-1,-1]

    -- compute the final stone arrangement by playing one stone per voter
    go :: Pile -> [Pile]
    go p = scanl step p [1..voters]
      where
        step :: Pile -> Voter -> Pile
        step p v  = turn p stone
          where
            Pile pp    = p
            (stone, _) = array A.! (v, pp)

    -- scanl :: (b -> a -> b) -> b -> [a] -> [b]


    -- keys are (voter,pile) and values are (stone to move, final pile)
    array :: A.Array (Int,Int) (Stone, Pile)
    array  = A.listArray bounds [ choice (v,p) | (v,p) <- A.range bounds ]
      where
        bounds  = ((1,1), (voters,piles))
        piles   = 8


    choice :: (Int,Int) -> (Stone, Pile)
    choice (v,p)
      | v == voters  = let st = snd $ minimumBy (comparing fst) [ (try  s, s) | s <- stones ]
                       in  (st, turn (pile p) st)

      | otherwise    = let st = snd $ minimumBy (comparing fst) [ (try2 s, s) | s <- stones ]
                           Pile p' = turn (pile p) st
                       in  (st, snd $ array A.! (v+1, p'))
      where
        stones :: [Stone]
        stones = stone <$> [1,2,3]

        try :: Stone -> Preference
        try s = preference v (turn (pile p) s)

        -- return our preference for the final pile if we choose this stone
        try2 :: Stone -> Preference
        try2 s = let Pile p' = turn (pile p) s            -- :: Pile
                     pp      = snd $ array A.! (v+1, p')  -- :: Pile
                 in  preference v pp


    preference :: Voter -> Pile -> Preference
    preference v (Pile p) = indexOf p (prefs !! (v-1))


turn :: Pile -> Stone -> Pile
turn pile (Stone s) = ltop $ go (ptol pile)
  where
    go [s1,s2,s3] | s == 1 = [ negate s1,        s2,        s3 ]
                  | s == 2 = [        s1, negate s2,        s3 ]
                  | s == 3 = [        s1,        s2, negate s3 ]

ptol :: Pile -> [Int]
ptol (Pile p)
  | p == 1    = [-1,-1,-1]
  | p == 2    = [-1,-1, 1]
  | p == 3    = [-1, 1,-1]
  | p == 4    = [-1, 1, 1]
  | p == 5    = [ 1,-1,-1]
  | p == 6    = [ 1,-1, 1]
  | p == 7    = [ 1, 1,-1]
  | p == 8    = [ 1, 1, 1]
  | otherwise = error "ptol"

ltop :: [Int] -> Pile
ltop [s1,s2,s3] = pile $ a1*(2^2) + a2*(2^1) + a3 + 1
  where
    a1 = bool 0 1 (s1 == 1)
    a2 = bool 0 1 (s2 == 1)
    a3 = bool 0 1 (s3 == 1)


{- Operations -}

uxuhul :: String -> String
uxuhul = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = uxuhul <$> readFile "test/inputs/uxuhul-3.input" >>= putStr

