{-  Amoebas (difficulty 1.7) - https://open.kattis.com/problems/amoebas

    This is an attempt at a single-pass algorithm for counting contiguous paths in a grid
    of pixels. It fails on the 8th kattis test. I probed around with various unit tests
    but it's passing everything I throw at it, so I can't seem to find where my logic is
    failing (kattis doesn't show the tests it's using)

    Revision: Fixing this was a matter of updating the trail to true up old path numbers
    with the lowest of the current pixel's immediate neighbours. The code is now clunkier
    though so a refactoring pass could improve readability

-}

module Amoebas (amoebas, try) where


{- Types -}

-- grid width, list of pixels (concatenated to one string)
type TestCase  = (Int, [Char])
data Output    = Output Int

instance Show Output where
  show (Output loops) = show loops


{- Parsing -}

{-  6 12
    .##########.
    #..........#
    .#..#.#....#
    #....#.....#
    #.........#.
    .#########..
-}
parse :: String -> TestCase
parse input = (read n, concat pixels)
  where
    (dims : pixels) = lines input
    (m : n : _)     = words dims



{- Methods -}

type Row   = Int
type Col   = Int
type Loop  = Int

data Pixel = Pixel { row  :: Row
                   , col  :: Col
                   , loop :: Loop     -- loop tag tentatively assigned
                   }

-- our code will traverse the grid from left to right, top to bottom, remembering the
-- coordinates and loop tag of each black pixel it comes across. loops can start in
-- different areas and meet only later, so in order to do this in a single pass we do our
-- best at joining as we go. when two paths meet, the current pixel will have one or two
-- neighbours with possibly different loop tags. we always pick the lowest one and assign
-- it to the current pixel, removing the other tags from the seen list since they were the
-- same loop the whole time and we don't want to over-count them

data Data  = Data { seen  :: [Loop]
                  , trail :: [Pixel]
                  , next  :: Int
                  }


process :: TestCase -> Output
process (width, pixels) = Output loops
  where
    loops         = length seen
    Data seen _ _ = foldl step start indexed

    start    = Data []    -- we've seen no loops so far
                    []    -- and haven't taken any steps yet
                    1     -- tag the loops starting at 1

    indexed  = zip [0..] pixels


    step :: Data -> (Int, Char) -> Data
    step (Data seen trail c) (i, '#') = Data seen' trail' c'
      where
        (row, col) = divMod i width
        neighbours = filter isNeighbour trail

        -- check only the 3 cells above and the one directly to the left. if there's one
        -- to the right of us it'll take the tag of this current one when we get there
        isNeighbour :: Pixel -> Bool
        isNeighbour (Pixel r c _) =  r == row      &&  c == col - 1
                                  || r == row - 1  &&  c `elem` [col-1,col,col+1]

        -- other pixels in our neighbourhood may have different loop tags,
        -- deconflict by selecting the lowest one and updating the trail to
        -- change old loop tags to the new one
        (loopID, c', seen', trail2)
                = if null neighbours
                  then (c , c+1,  c : seen, trail)
                  else (ma, c  , ma : filter (`notElem` around) seen
                               , map update trail)
          where
            update :: Pixel -> Pixel
            update pixel
              | loop pixel `elem` around = pixel { loop = ma }
              | otherwise                = pixel

        ma      = minimum around
        around  = map loop neighbours

        -- trim expired steps from the trail and add on this latest step
        trail'  = trimmed ++ [Pixel row col loopID]
        trimmed = dropWhile expired trail2

        -- true if we've moved sufficiently beyond a coord and won't need it further
        expired :: Pixel -> Bool
        expired (Pixel r c _) =  r <= row - 2

                              -- this will over-trim to the left... we need them there
                              -- to be above when we get to the cells below them
                              -- (still fails on test 8 though)
                              --
                              -- || r == row - 1  &&  c <= col - 2


    -- no loop on this step so our accumulated data doesn't change
    step d (_, '.') = d



{- Operations -}

amoebas :: String -> String
amoebas = show . process . parse

try :: IO ()
try = amoebas <$> readFile "test/inputs/amoebas-1.input" >>= putStrLn

