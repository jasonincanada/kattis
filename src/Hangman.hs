{-  Hangman (difficulty 1.5) - https://open.kattis.com/problems/hangman -}

module Hangman (hangman, try) where


{- Types -}

-- hidden word, letters left to try
type TestCase  = ([Char], [Char])

data Outcome   = Win | Lose
data Output    = Output Outcome

instance Show Output where
  show (Output Win ) = "WIN"
  show (Output Lose) = "LOSE"


{- Parsing -}

{-  HANGMAN
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
-}
parse :: String -> TestCase
parse input = (word, alphabet)
  where
    (word : alphabet : _) = lines input



{- Methods -}

attempts = 10

process :: TestCase -> Output
process (word, alphabet) = Output outcome
  where
    outcome = go word alphabet ""

    go :: [Char] -> [Char] -> [Char] -> Outcome
    go word (a:as) hangman

      | null word                   = Win  -- we win if there are no letters
                                           -- in the word left to guess

      | length hangman == attempts  = Lose -- we lose if they drew the whole
                                           -- hangman figure

      -- if we guess a letter in the word, remove all occurrences
      -- of the letter from it and recurse on the rest of the guesses
      | a `elem` word  = go (filter (/=a) word) as hangman

      -- if it's not in the word, draw another stick onto the figure
      | otherwise      = go word as (hangman ++ "-")



{- Operations -}

hangman :: String -> String
hangman = show . process . parse

try :: IO ()
try = hangman <$> readFile "inputs/hangman-1.input" >>= putStrLn

