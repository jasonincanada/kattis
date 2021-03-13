
{-  Death Knight Hero (difficulty 1.7) - https://open.kattis.com/problems/deathknight -}

module DeathKnight (deathknight, try) where

import Data.Function ((&))


{- Types -}

data Spell = ChainsOfIce
           | DeathGrip
           | Obliterate


{- Parsing -}

parse :: String -> [[Spell]]
parse = map (map toSpell) . tail . lines

toSpell :: Char -> Spell
toSpell 'C' = ChainsOfIce
toSpell 'D' = DeathGrip
toSpell 'O' = Obliterate


{- Methods -}

process :: [[Spell]] -> Int
process spells = filter (not . check) spells
                   & length
  where
    check :: [Spell] -> Bool
    check spells = zipWith compare spells (tail spells)
                     & or

    compare :: Spell -> Spell -> Bool
    compare ChainsOfIce DeathGrip = True
    compare _ _                   = False


deathknight :: String -> String
deathknight = show . process . parse


{- Operations -}

try :: IO ()
try = deathknight <$> readFile "inputs/deathknight.input" >>= putStrLn

