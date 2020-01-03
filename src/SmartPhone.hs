module SmartPhone (smartphone, try) where

{-  SmartPhone (difficulty 2.7) - https://open.kattis.com/problems/smartphone -}

import Control.Arrow ((>>>))
import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.Writer
import Data.Function ((&))
import Scanner


{- Types -}

type Goal       = String
type TypedSoFar = String
type Suggestion = String

type TestCase   = (Goal, TypedSoFar, [Suggestion])

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Output Int

instance Show Output where
  show (Output count) = show count


{- Parsing -}

{-  1
    CAKEEATER
    CAK
    CAKEMONSTER
    CARNIVAL
    CAKEEATUR
-}
parseInput :: Scanner [TestCase]
parseInput = numberOf parseCase

parseCase :: Scanner TestCase
parseCase = (,,) <$> str        -- goal
                 <*> str        -- typed so far
                 <*> three str  -- suggestions


{- Logic -}

-- Use a monad stack to operate on a String while tracking the cost (using monoidal Sum)
--
-- runWriterT :: WriterT w m a -> m (a, w)
--
type Operation = WriterT (Sum Int) (State String) ()


doCase :: TestCase -> Output
doCase (goal, typedSoFar, suggestions) = Output quickest
  where
    quickest = minimum costs


    -- build the 4 possible options for completing the word
    options :: [Operation]
    options = plain : (suggest <$> suggestions)
      where
        plain     =          backspace >> typeRest
        suggest s = use s >> backspace >> typeRest


    -- measure the cost of each option
    costs :: [Int]
    costs = try <$> options 
      where
        try option = typedSoFar
                       & evalState (runWriterT option)
                       & snd
                       & getSum


    -- backspace until our word is a prefix of the goal word
    backspace :: Operation
    backspace = do
      word <- lift get

      let new  = commonPrefix word goal
      let cost = length word - length new

      lift $ put new
      tell $ Sum cost


    -- type the rest of the word (assumes a common prefix already)
    typeRest :: Operation
    typeRest = do
      word <- lift get

      let cost = length goal - length word

      lift $ put goal
      tell $ Sum cost


    -- replace the word with a suggestion
    use :: String -> Operation
    use suggestion = lift (put suggestion) >> tell (Sum 1)


-- Get the longest common prefix of two strings
commonPrefix :: String -> String -> String
commonPrefix "" _  = ""
commonPrefix _ ""  = ""
commonPrefix (s:ss) (t:tt)
  | s == t    = s : commonPrefix ss tt
  | otherwise = ""



{- Operations -}

smartphone :: String -> String
smartphone = parse >>> process >>> combine
  where
    parse   = runLineScanner parseInput
    process = map (doCase >>> show)
    combine = unlines


-- For running in the GHCi repl
try :: IO ()
try = smartphone <$> readFile "inputs/smartphone.input" >>= putStr

