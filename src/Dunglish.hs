module Dunglish (dunglish, try) where

{-  Dunglish (difficulty 2.3) - https://open.kattis.com/problems/dunglish

    This is a straightforward challenge using sums and products. We'll focus on the
    monoidal structure of the sub-problems and forego any data structure more
    complicated than the built-in list.

    The three monoids used in this solution: Sum, Product, All

      Sum 3     <> Sum 5     = Sum 8        -- mempty = Sum 0
      Product 3 <> Product 5 = Product 15   -- mempty = Product 1

      All True  <> All True = All True      -- mempty = All True
      All False <> All True = All False


    But actually, there's a fourth one: the built-in list!

      ["loose", "lips"] <> ["sink", "ships"] = ["loose", "lips", "sink", "ships"]
      ["foo"]           <> []                = ["foo"]

    Any two monoids can be paired up and the whole pair forms a monoid. We use this
    to our advantage in doCase to translate a sentence while tracking whether every
    translated word is a correct one--thereby making the whole sentence correct.


    The get* functions pull the value out so we can use them with regular functions:

      getAll (All True)       = True
      getProduct (Product 15) = 15
      getSum (Sum 8)          = 8


    We have thrown performance concerns to the wind with this one! There is a lot of
    repetitive traversal of the dictionary, but we are focusing on the monoidal
    sub-structures for this challenge instead of finding a fast solution. A faster
    solution would use Data.Map and Data.Set for quicker lookups, and exponentiation
    instead of "repeated multiplication" as we are effectively doing with the
    Product type when we have repeated words in a sentence.

-}

import Prelude hiding (Word)
import Control.Arrow  ((>>>))
import Control.Monad  (replicateM)
import Data.Bool      (bool)
import Data.Function  ((&))
import Data.Monoid    (All(..), Product(..), Sum(..))
import Scanner


{- Types -}

type Word       =  String
type Sentence   = [Word]
type Record     = (Word, Word, Bool)        -- dutch word, english word, and correct/incorrect
type Dictionary = [Record]
type TestCase   = (Sentence, Dictionary)

data Output     = Tally Int Int             -- counts of correct and incorrect translations
                | Translation Sentence Bool -- the first full translation, either correct or incorrect

instance Show Output where
  show (Tally correct incorrect) = unlines [ show correct   ++ " correct",
                                             show incorrect ++ " incorrect" ]
  show (Translation sentence c)  = unlines [ unwords sentence,
                                             bool "incorrect" "correct" c ]

{- Parsing -}

{-  7
    als mollen mollen mollen mollen mollen mollen
    4
    als when correct
    mollen moles correct
    mollen destroy correct
    mollen mills incorrect
-}
parseInput :: Scanner TestCase
parseInput = do
  sentence <- runWordScanner (many str) <$> (str >> str)
  records  <- numberOf parseRecord

  return (sentence, records)


{- mollen moles correct -}
parseRecord :: Scanner Record
parseRecord = do
  [dutch, english, correctness] <- runWordScanner (many str) <$> str

  return (dutch, english, correctness == "correct")


{- Logic -}

-- Map a word to the number of correct translations in the dictionary
countCorrect :: Dictionary -> Word -> Int
countCorrect dict word = foldMap f dict & getSum
  where
    f :: Record -> Sum Int
    f (dutch, _, correct)
      | dutch == word && correct = Sum 1
      | otherwise                = Sum 0


-- Map a word to the total number of translations
countTotal :: Dictionary -> Word -> Int
countTotal dict word = foldMap f dict & getSum
  where
    f :: Record -> Sum Int
    f (dutch, _, _)
      | dutch == word = Sum 1
      | otherwise     = Sum 0


doCase :: TestCase -> Output
doCase (sentence, dict) = output
  where
    correct   = foldMap f sentence & getProduct
    total     = foldMap g sentence & getProduct
    incorrect = total - correct

    f, g :: Word -> Product Int
    f word = Product (countCorrect dict word)
    g word = Product (countTotal   dict word)

    output = if total == 1
             then Translation inEnglish allCorrect
             else Tally correct incorrect

    (inEnglish, allCorrect) = foldMap (first dict) sentence & fmap getAll

    -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m


    -- Get the first translation of a word in the dictionary. The found word is
    -- put in a singleton list so it--and the resulting tuple!--forms a monoid
    first :: Dictionary -> Word -> ([Word], All)
    first ( (dutch,english,correct) : ws ) word
      | dutch == word = ([english], All correct)
      | otherwise     = first ws word


{- Operations -}

dunglish :: String -> String
dunglish = parse >>> process >>> show
  where
    parse   = runLineScanner parseInput
    process = doCase


-- For running in the GHCi repl
try :: IO ()
try = dunglish <$> readFile "inputs/dunglish-2.input" >>= putStr

