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

two, four, six, eight :: Scanner a -> Scanner [a]
[two, four, six, eight] = map replicateM [2,4,6,8]


-- I added these for convenience
runWordScanner = runScanner words
runLineScanner = runScanner lines

