{-  Bradspelet - https://open.kattis.com/problems/bradspelet  -}

module Bradspelet (bradspelet, cuts) where

import qualified Data.Array as A

data TestCase  = TestCase Int Int
data Output    = Output Bool

instance Show Output where
  show (Output True ) = "A"
  show (Output False) = "B"


parse :: String -> TestCase
parse input = TestCase (min n m) (max n m)
  where
    n       = read a
    m       = read b
    (a:b:_) = words . head . lines $ input

-- n < m
process :: TestCase -> Output
process (TestCase n m) = Output $ array A.! (n,m)
  where
    array :: A.Array (Int, Int) Bool
    array = A.listArray bounds [ winfrom n m | (n,m) <- A.range bounds ]
      where
        bounds = ((1,1), (n,m))

    winfrom :: Int -> Int -> Bool
    winfrom 1 1 = False
    winfrom n m
      | n > m     = winfrom m n

      -- the player wins with a block if they can cut it in such a way the other player can't win
      -- with either of the remaining two blocks
      | otherwise = or [ losefrom n m && losefrom n' m' | ((n,m), (n',m')) <- cuts n m ]
      where
        losefrom n m = not (array A.! (n,m))


cuts :: Int -> Int -> [((Int,Int), (Int,Int))]
cuts 1 1 = undefined
cuts n m
  | n > m   = cuts m n

  -- squares
  | n == m  = [ ((across, m), (n - across, m)) | across <- [1 .. n `div` 2]]

  -- rectangles
  | n < m   = [ ((across, m), (n - across, m)) | across <- [1 .. n `div` 2]] ++
              [ ((n, down)  , (n, m - down))   | down   <- [1 .. m `div` 2]]


bradspelet :: String -> String
bradspelet = show . process . parse

