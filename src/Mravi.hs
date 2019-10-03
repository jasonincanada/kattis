{-# Language RecordWildCards #-}

module Mravi (mravi, try) where

{-  Mravi (difficulty 2.7) - https://open.kattis.com/problems/mravi -}

import           Control.Arrow ((>>>), (&&&))
import           Control.Monad (replicateM)
import qualified Data.IntMap as IM
import           Scanner


{- Types -}

type Label    = Int            -- nodes are labeled with integers, 1 being the root
type Flow     = Double         -- measure of water flow
type Portion  = Double         -- percentage of the prior node's flow (0.01 ≤ x ≤ 1)
type Req      = Double         -- required amount of liquid at a leaf node

data Pipe     = Pipe { from    :: Label
                     , to      :: Label
                     , portion :: Portion
                     , super   :: Bool
                     }

type PipeMap  = IM.IntMap Pipe -- keyed on the target node (nodes may have many
                               -- outflows but only one inflow so this is safe)

type TestCase = (PipeMap, [(Label, Req)])

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output   = Output Flow

instance Show Output where
  show (Output flow) = show flow


{- Parsing -}

{-  3
    1 2 20 1
    1 3 80 1
    -1 4 8
-}
parseInput :: Scanner TestCase
parseInput = do
  count <- int
  pipes <- replicateM (count-1) parsePipe
  reqs  <- runWordScanner (many int) <$> str

  return (mapped pipes, indexed reqs)

  where
    mapped :: [Pipe] -> PipeMap
    mapped = map (to &&& id) >>> IM.fromList

    indexed :: [Int] -> [(Label, Req)]
    indexed = map fromIntegral >>> zip [1..] >>> filter (snd >>> (>=0))


-- 1 2 20 1
parsePipe :: Scanner Pipe
parsePipe = do
  [from, to, portion, super] <- runWordScanner (four int) <$> str
  return $ Pipe from to (fromIntegral portion / 100) (super == 1)


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
        path label = pipe : path (from pipe)
          where
            pipe = pipes IM.! label

        -- convert this pipe into its mini-function
        k :: Pipe -> (Flow -> Flow)
        k Pipe{..}
          | super     = sqrt >>> (/portion)
          | otherwise =          (/portion)

        -- string the mini-functions together into one big function
        compose :: [Flow -> Flow] -> (Req -> Flow)
        compose = foldl (>>>) init
          where
            init :: Req -> Flow
            init = id


{- Operations -}

mravi :: String -> String
mravi = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = mravi <$> readFile "inputs/mravi-3.input" >>= putStr

