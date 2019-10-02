module Mravi (mravi, try) where

{-  Mravi (difficulty 2.7) - https://open.kattis.com/problems/mravi -}

import           Control.Arrow ((>>>))
import           Control.Monad (replicateM)
import qualified Data.IntMap as IM
import           Scanner


{- Types -}

type Label    = Int            -- nodes are labeled with integers, 1 being the root
type Flow     = Double         -- percentage of the prior node's flow (1 ⇐ x ⇐ 100)
type Req      = Double         -- required amount of liquid at a leaf node

type Pipe     = (Label, Label, Flow, Bool)

type PipeMap  = IM.IntMap Pipe -- keyed on the target node (nodes may have many
                               -- outflows but only one inflow so this is safe)

type TestCase = (PipeMap, [(Label, Req)])

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output   = Output Flow

instance Show Output where
  show (Output flow) = show flow


{- Parsing -}

parseInput :: Scanner TestCase
parseInput = do
  count <- int
  pipes <- replicateM (count-1) parsePipe
  reqs  <- runWordScanner (many int) <$> str

  return (mapped pipes, indexed reqs)

  where
    mapped :: [Pipe] -> PipeMap
    mapped = map (\pipe@(_,to,_,_) -> (to, pipe)) >>> IM.fromList

    indexed :: [Int] -> [(Label, Req)]
    indexed = map fromIntegral >>> zip [1..] >>> filter (snd >>> (>=0))


parsePipe :: Scanner Pipe
parsePipe = do
  [from, to, flow, super] <- runWordScanner (four int) <$> str
  return (from, to, fromIntegral flow, super == 1)


{- Logic -}

doCase :: TestCase -> Output
doCase (pipes, reqs) = Output x
  where
    x   = maximum xs
    xs  = map (uncurry buildFunction) reqs    -- reqs :: [ (Label, Req) ]

    buildFunction :: Label -> (Req -> Flow)
    buildFunction = path >>> map k >>> compose
      where
        -- construct the path of pipes from the given leaf to the root
        path :: Label -> [Pipe]
        path 1     = []
        path label = pipe : rest
          where
            pipe         = pipes IM.! label
            rest         = path from
            (from,_,_,_) = pipe

        -- convert this pipe into its mini-function
        k :: Pipe -> (Flow -> Flow)
        k (_, _, flow, super)
          | super     = sqrt >>> (/(flow/100))
          | otherwise =          (/(flow/100))

        -- string the mini-functions together into one big function
        compose :: [(Flow -> Flow)] -> (Flow -> Flow)
        compose = foldl (>>>) id


{- Operations -}

mravi :: String -> String
mravi = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = mravi <$> readFile "inputs/mravi-3.input" >>= putStr

