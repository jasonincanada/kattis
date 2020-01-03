{-# Language LambdaCase #-}

{- Most of the code in this file is from:

   https://byorgey.wordpress.com/2019/05/22/competitive-programming-in-haskell-scanner/
-}

module Scanner where

import Control.Monad.State


type Scanner a = State [String] a

runScanner :: (String -> [String]) -> Scanner a -> String -> a
runScanner f s = evalState s . f

str :: Scanner String
str = get >>= \case s:ss -> put ss >> return s

int :: Scanner Int
int = read <$> str

count :: Scanner Int
count = read <$> str

float :: Scanner Float
float = read <$> str
  
numberOf :: Scanner a -> Scanner [a]
numberOf s = count >>= flip replicateM s

many :: Scanner a -> Scanner [a]
many s = get >>= \case { [] -> return [];
                         _  -> (:) <$> s <*> many s }

two, three, four, six, eight :: Scanner a -> Scanner [a]
[two, three, four, six, eight] = map replicateM [2,3,4,6,8]


-- I added the following
runWordScanner = runScanner words
runLineScanner = runScanner lines

parseLine :: Scanner a -> Scanner a
parseLine p = runWordScanner p <$> str

triple :: Scanner a -> Scanner (a, a, a)
triple s =
  three s >>= \case
                 [a,b,c] -> return (a,b,c)
                 _       -> error "expecting exactly three elements"


bigint :: Scanner Integer
bigint = read <$> str

