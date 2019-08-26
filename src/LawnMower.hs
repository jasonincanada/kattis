module LawnMower (lawnmower, try) where

{-  LawnMower (difficulty 2.1) - https://open.kattis.com/problems/lawnmower -}

import Control.Arrow ((>>>))
import Data.Bool     (bool)
import Scanner


{- Types -}

type Strip      = (Float, Float)  -- closed interval between two offsets
type Lawn       = Strip           -- represent the starting lawn as one unmowed strip
type Mower      = Float           -- width of the mower
type CenterLine = Float           -- x or y offset, the mower is centered on this

--                (width, horizontals , verticals   )
type TestCase   = (Mower, [CenterLine], [CenterLine])

-- The output object will be different for each challenge.  It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Yes | No

instance Show Output where
  show Yes = "YES"
  show No  = "NO"


{- Parsing -}

parseInput :: Scanner [TestCase]
parseInput = many parseTestCase

{-  8 11 10.0
    0.0 10.0 20.0 30.0 40.0 50.0 60.0 70.0
    0.0 10.0 20.0 30.0 40.0 50.0 60.0 70.0 80.0 90.0 100.0
-}
parseTestCase :: Scanner TestCase
parseTestCase = do
  width <- runWordScanner (str >> str >> float) <$> str

  if width /= 0.0
    then do
      horizontals <- runWordScanner (many float) <$> str
      verticals   <- runWordScanner (many float) <$> str
      return (width, horizontals, verticals)
      
    else
      return (0, [], [])


{- Logic -}

-- Not all test cases may need to be run; there is sometimes an "end-of-input" marker case that we should skip
isValid :: TestCase -> Bool
isValid (width, _, _) = width > 0.0


-- Constants given by the problem description
lawnLength, lawnWidth :: Lawn
lawnLength = (0, 100)
lawnWidth  = (0, 75 )


-- Return the unmowed strips left on a lawn after a mower mows the given list of strips
mow :: Lawn -> [Strip] -> [Strip]
mow lawn = cut >>> prune 
  where
    cut   = foldr go [lawn]
    prune = filter (uncurry (/=)) -- remove infinitely thin strips, eg (5.0, 5.0)

    go :: Strip -> [Strip] -> [Strip]
    go strip unmowed = foldMap (overlay strip) unmowed

    -- foldr   ::  Foldable t            => (a -> b -> b) -> b -> t a -> b
    -- foldMap :: (Foldable t, Monoid m) => (a -> m)           -> t a -> m


-- What happens, if anything, to an arbitrary unmowed strip (c, d) when we mow a strip (a, b).
-- The logic below leaves strips of 0 width which are cleaned up in the pruning step
overlay :: Strip -> Strip -> [Strip]
overlay (a, b) (c, d)

  -- if this pass fully mows this strip, remove it from the list
  | a <= c && b >= d = []

  -- if there is no overlap, leave the strip
  | b < c            = [ (c, d) ]
  | a > d            = [ (c, d) ]
  
  -- if this pass lies fully within the unmowed strip, remove it and leave the strips on either side
  | a >= c && b <= d = [ (c, a), (b, d) ] 

  -- otherwise it covers only some of the strip, leaving one smaller strip behind
  | a >= c           = [ (c, a) ]
  | b <= d           = [ (b, d) ]


doCase :: TestCase -> Output
doCase (width, horizontals, verticals) = fullyMowed
  where
    fullyMowed  = bool No Yes (null mowedAcross && null mowedUp)

    mowedAcross = mow lawnWidth  $ map (strip width) horizontals
    mowedUp     = mow lawnLength $ map (strip width) verticals

    -- The strip a mower with width w makes along center line at offset x
    strip :: Mower -> CenterLine -> Strip
    strip w x = (x - w/2, x + w/2)


{- Operations -}

-- The main function that maps the whole input file to all test results
lawnmower :: String -> String
lawnmower = parse >>> validate >>> process >>> combine
  where
    parse    = runLineScanner parseInput
    validate = filter isValid
    process  = map (doCase >>> show)
    combine  = unlines


-- For running in the GHCi repl
try :: IO ()
try = lawnmower <$> readFile "inputs/lawnmower.input" >>= putStr

