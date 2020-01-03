module RotateCut (rotatecut, try) where

{-  RotateCut (difficulty 3.0) - https://open.kattis.com/problems/rotatecut

-}

import Control.Arrow ((>>>))
import Scanner


{- Types -}

type TestCase = (Int, String)

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output   = Output String

instance Show Output where
  show (Output sentence) = sentence


{- Parsing -}

{-  2
    1 IWantToSleep
    2 ZZZ
-}
parseInput :: Scanner [TestCase]
parseInput = numberOf $ parseLine p
  where
    p = (,) <$> int -- number of operations
            <*> str -- original sentence



{- Logic -}

type State = ( Int  -- left index
             , Int  -- right index
             , Int  -- number of cuts left
             , Bool -- if the next cut is on the left side of the string
             )

doCase :: TestCase -> Output
doCase (num, sentence) = Output result
  where
    result = go start
    start  = (0, length sentence, num, True)

    go :: State -> String
    go (left, right, count, side)
      | right - left <= 3 = answer left right
      | count == 0        = answer left right
      | side  == True     = go (left+size, right     , count-1, not side)
      | side  == False    = go (left     , right-size, count-1, not side)
      where
        size = (right-left) `div` 4

    -- Do the actual "combined" cuts once
    answer l r = take (r-l) $ drop l sentence



{- Operations -}

rotatecut :: String -> String
rotatecut = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = rotatecut <$> readFile "inputs/rotatecut.input" >>= putStr

