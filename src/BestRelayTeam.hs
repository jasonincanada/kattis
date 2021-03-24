{-  Best Relay Team (difficulty 1.8) - https://open.kattis.com/problems/bestrelayteam

    The key to solving this challenge efficiently is to sort the pool of runners ahead of
    time by their 2nd leg times. Regardless of which runner is chosen for the 1st leg, we
    always want the 3 fastest 2nd-leg runners from the rest of the pool. Because we only
    need 3 runners after removing the first one, and the list is already sorted, it's a
    constant-time operation to find the other 3 and complete the team build. So this
    reduces to a linearithmic-time sort followed by a linear-time traversal to try out
    each runner as the first leg

    The overall complexity of this solution is O(n log n + 3n). The runtime is 0.01s but
    the Haskell leaderboard is already full
-}

module BestRelayTeam (bestrelayteam, try) where

import Data.List (delete, minimumBy, sortOn)


{- Types -}

data Runner    = Runner { name  :: Name
                        , leg1  :: Int
                        , leg2  :: Int    -- legs 2-4
                        } deriving Eq

type Name      = String
type Team      = [Runner]

type TestCase  = [Runner]
data Output    = Output Team

instance Show Output where
  show (Output team) = unlines (show floating : map name team)
    where
      floating = fromIntegral (time team) / 100


{- Parsing -}

{-  4
    ASHMEADE 9.90 8.85
    BLAKE 9.69 8.72
    BOLT 9.58 8.43
    CARTER 9.78 8.93
-}
parse :: String -> TestCase
parse = map (runner . words) . tail . lines

runner :: [String] -> Runner
runner (name : leg1 : leg2 : _) = Runner name
                                         (read $ filter (/= '.') leg1)
                                         (read $ filter (/= '.') leg2)


{- Methods -}

process :: TestCase -> Output
process runners = Output fastest
  where
    fastest :: Team
    fastest = minBy time teams

    teams :: [Team]
    teams = map asFirst runners
      where

        -- make this runner the first on the team and fill it out with the 3 fastest
        -- 2nd-leggers that aren't this runner
        asFirst :: Runner -> Team
        asFirst runner = runner : take (4-1) (delete runner sorted)

        -- sort the runners by their 2nd leg time. this list is generated once
        -- and reused in each call to asFirst
        sorted :: [Runner]
        sorted = sortOn leg2 runners


-- the total time for a team to complete the race
time :: Team -> Int
time (r:rs) = leg1 r + sum (map leg2 rs)


-- find the minimum of a list of values based on a custom measurer f
minBy :: Ord a => (b -> a) -> [b] -> b
minBy f = minimumBy cmp
  where
    cmp a b = f a `compare` f b


{- Operations -}

bestrelayteam :: String -> String
bestrelayteam = show . process . parse

try :: IO ()
try = bestrelayteam <$> readFile "test/inputs/bestrelayteam-1.input" >>= putStrLn

