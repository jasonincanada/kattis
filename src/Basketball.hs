{-# Language TupleSections #-}

module Basketball (basketball, try) where

{-  Basketball (difficulty 2.7) - https://open.kattis.com/problems/competitivearcadebasketball

    An easy one we're doing just to put a Haskell solution on the leaderboard and see
    where it ranks in terms of running time compared to other languages.

    Result: It ranks horribly, at 1.3s
-}

import           Control.Arrow ((>>>))
import           Control.Monad (replicateM)
import           Data.Bool     (bool)
import qualified Data.DList      as DL
import qualified Data.Map.Strict as M
import           Scanner


{- Types -}

type Player     = String
type Points     = Int

               -- (required points to win, list of players, list of points scored)
type TestCase   = (Points, [Player], [(Player, Points)])

-- The output object will be different for each challenge. It must instantiate Show so it
-- can be converted to a string for the solution checker
data Output     = Nobody
                | Winners [Player]

instance Show Output where
  show Nobody       = "No winner!"
  show (Winners ws) = unlines $ map (++" wins!") ws


{- Parsing -}

{-  3 10 2
    John
    Kelly
    George
    Kelly 1
    George 2
-}
parseInput :: Scanner TestCase
parseInput = do
  [players, goal, count] <- runWordScanner (many int) <$> str
  names  <- replicateM players str
  scores <- replicateM count   parseScore

  return (goal, names, scores)

parseScore :: Scanner (Player, Points)
parseScore = runWordScanner ((,) <$> str <*> int) <$> str


{- Logic -}

--           (running tallies,  list of winners) 
type State = (M.Map Player Int, DL.DList Player)

doCase :: TestCase -> Output
doCase (goal, players, scores) = result
  where
    dict   = M.fromList $ map (,0) players
    start  = (dict, DL.empty)
    folded = foldl f start scores

    result = if null $ DL.toList $ snd folded
             then Nobody
             else Winners $ DL.toList $ snd folded

    f :: State -> (Player, Points) -> State
    f (dict, winners) (player, points) = (dict', winners')
      where
        dict'    = M.adjust (+points) player dict
        newWin   = running < goal && running + points >= goal
        winners' = bool winners (winners `DL.snoc` player) newWin
        running  = dict M.! player


{- Operations -}

basketball :: String -> String
basketball = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = basketball <$> readFile "inputs/basketball-2.input" >>= putStr

