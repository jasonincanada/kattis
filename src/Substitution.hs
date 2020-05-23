module Substitution (try, substitution) where

{-  The Power of Substitution (difficulty 8.0) - https://open.kattis.com/problems/substitution

    This is a challenge picked out by Brent Yorgey for his blog followers; see the "Next
    problem" section at:

      https://byorgey.wordpress.com/2020/05/22/competitive-programming-in-haskell-building-unordered-trees/

    My first attempt was the obvious naive solution that does a complete enciphering of
    the plaintext on each iteration, keeping track of the number of rounds required to
    arrive at the final ciphertext. As expected, this times out on the kattis server. So
    even though it passes on the sample inputs I'm not sure if it's correct because the
    correctness check would happen after the program finishes--but it doesn't in time.

    After staring at the problem a bit I found a way to reduce the execution time from
    >1.00s down to 0.02s, though it doesn't give correct answers to the kattis tests. It
    does pass the sample tests, and of course kattis doesn't show you the inputs it's
    actually running on the server, which makes it really hard to debug what's going on.

    The insight is to decompose the permutation represented by the substitution table into
    its cycles, calculate the "distance" within a cycle from a starting number to another
    number within that cycle, then take the lowest common multiple of the distances from
    each plaintext number to its expected enciphered number. But in the code I quickly
    dropped the cycle decomposition idea and went with what I *think* is equivalent: a
    lazily-evaluated array, keyed on a (from,to) pair with values being the enciphering
    "distance" between the pair. This is easy to recursively define as the interaction
    between `array` and the `distance` function, which mutually refer to each other. 
    
    Both approaches lean on the idea that each column in the message is independent of the
    other columns, but that their distances do have to work together (via their lcm) to
    arrive at their final enciphered number at the same time.

    This method does pass the sample tests but doesn't pass the second test on the kattis
    server. So either I'm wrong about the correctness of this method or there's a bug in
    the implementation somewhere.

-}

import Control.Arrow ((>>>))
import qualified Data.Array  as A
import qualified Data.IntMap as IM

import Scanner


{- Types -}

type Message   = [Int]
type Cipher    = [Int]
type Table     = IM.IntMap Int
type TestCase  = (Message, Cipher, Table)

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output    = Output Int

instance Show Output where
  show (Output k) = show k


{- Parsing -}

-- 1
-- 3
-- 1 2 3
-- 4 5 6
-- 2 3 4 5 6 7 8 9 10 ...
parseInput :: Scanner [TestCase]
parseInput = numberOf test
  where
    test :: Scanner TestCase
    test = do
      _       <- int
      message <- parseLine (many int)
      cipher  <- parseLine (many int)
      table   <- parseLine (many int)

      let intmap = IM.fromList $ zip [1..] table

      return (message, cipher, intmap)



{- Methods -}

-- much more efficient implementation than doCaseA, however, it is not correct; it gives
-- the wrong answer to the tests on kattis
doCase :: TestCase -> Output
doCase (plaintext, goal, table) = Output result
  where
    result    = foldl1 lcm distances
    distances = map (array A.!) (zip plaintext goal)
    
    array :: A.Array (Int,Int) Int
    array  = A.listArray bounds [ distance from to | (from,to) <- A.range bounds ]
      where
        bounds = ((1,1), (100,100))

    distance :: Int -> Int -> Int
    distance from to
      | from == to  = 0
      | otherwise   = 1 + array A.! (encrypt from, to)

    encrypt from = table IM.! from


-- naive implementation (times out on kattis, so it may not even be correct)
doCaseA :: TestCase -> Output
doCaseA (plaintext, goal, table) = Output result
  where
    result = go 1 plaintext

    go :: Int -> Message -> Int
    go n message
      | cipher == goal = n
      | otherwise      = go (n+1) cipher
      where
        cipher = encrypt message

        encrypt :: Message -> Cipher
        encrypt = map (\m -> table IM.! m)



{- Operations -}

substitution :: String -> String
substitution = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = substitution <$> readFile "test/inputs/substitution-1.input" >>= putStr

