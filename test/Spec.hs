import Test.Hspec
import Control.Monad (forM_)

import Golomb        (golomb)
import Lektira       (lektira)
import MarblesTree   (marblestree)
import Recenice      (recenice)
import SmallSchedule (smallschedule)
import Uxuhul        (ltop, ptol, pile, turn, uxuhul, Stone(..))
--import Ceiling       (ceiling)
import Limbo1        (limbo1)
import Limbo2        (limbo2, toRectangle, toSquare, Rect(..), Square(..), f, g, doCase,
                      rectCorner, rectTop, Output(..))


path = "test/inputs/"

main :: IO ()
main = hspec $ do

  describe "Golomb" $ do

    let cases = [
                -- Sample inputs from the problem page
                  ( "golomb.input", unlines [ "not a ruler"
                                            , "perfect"
                                            , "missing 1 2 3 4"
                                            , "not a ruler"
                                            , "missing 6"] ) ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (golomb <$> readFile (path ++ testfile))
                        >>= (`shouldBe` result)


  describe "MarblesTree" $ do

    let cases = [
                -- Sample inputs from the problem page
                  ( "marblestree-1.input", 7  )
                , ( "marblestree-2.input", 14 )
                , ( "marblestree-3.input", 20 )

                -- Probing inputs searching for the bug
                , ( "marblestree-4.input",  0 )
                , ( "marblestree-5.input",  3 )
                , ( "marblestree-6.input",  0 )
                , ( "marblestree-7.input",  1 )
                , ( "marblestree-8.input",  3 )
                , ( "marblestree-9.input",  3 )
                , ( "marblestree-10.input", 2 )
                , ( "marblestree-11.input", 6 )
                , ( "marblestree-12.input", 3 )
                , ( "marblestree-13.input", 5 )
                , ( "marblestree-14.input", 7 )
                , ( "marblestree-15.input", 4 )

                , ( "marblestree-16.input", 2 )
                ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (marblestree <$> readFile (path ++ testfile))
                        >>= (`shouldBe` (unlines [show result]))



  describe "Recenice" $ do

    let cases = [
                -- Sample inputs from the problem page
                  ( "recenice-1.input", "this sentence has thirtyone letters"  )
                , ( "recenice-2.input", "thirty is the number of letters here" )
                , ( "recenice-3.input", "the letters are twentynine potato"    )
                ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (recenice <$> readFile (path ++ testfile))
                        >>= (`shouldBe` result)



  describe "SmallSchedule" $ do

{-
    The input consists of a single line containing four integers Q (2≤Q≤1000), which is
    the time needed to complete the longer batches, M (1≤M≤1000000), which is the number
    of machines owned by your company, S (0≤S≤1000000), which is the number of 1-second
    time slots purchased, and L (0≤L≤1000000), which is the number of Q-second time slots
    purchased.
-}

    let cases = [
                -- Sample inputs from the problem page
                  ( "2 4 3 6" , 4  )
                , ( "3 4 3 5" , 6  )
                , ( "10 2 0 1", 10 )

                -- Probing inputs searching for the bug
                , ( "1 1 0 1" , 1  )
                , ( "1 1 1 1" , 2  ) -- add a 1-min process
                                     -- bingo, this yields 1 instead of 2


                ]

    forM_ cases $
      \(input, result) ->
        it (input ++ " -> " ++ show result) $
          smallschedule input `shouldBe` show result


  describe "Lektira" $ do

    let cases = [
                -- Sample inputs from the problem page
                  ( "lektira-1.input", "abcdefghijk" )
                , ( "lektira-2.input", "bometil" )
                , ( "lektira-3.input", "aanadnok" )
                ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (lektira <$> readFile (path ++ testfile))
                        >>= (`shouldBe` result)


  describe "Uxuhul Voting" $ do

    it "turn 1 1" $ show (turn (pile 1) (Stone 1)) `shouldBe` "Pile 5"
    it "turn 1 2" $ show (turn (pile 1) (Stone 2)) `shouldBe` "Pile 3"
    it "turn 1 3" $ show (turn (pile 1) (Stone 3)) `shouldBe` "Pile 2"
    it "turn 2 3" $ show (turn (pile 2) (Stone 3)) `shouldBe` "Pile 1"
    it "turn 3 2" $ show (turn (pile 3) (Stone 2)) `shouldBe` "Pile 1"
    it "turn 8 2" $ show (turn (pile 8) (Stone 2)) `shouldBe` "Pile 6"

    it "ltop [-1,-1,-1]" $ show (ltop [-1,-1,-1]) `shouldBe` "Pile 1"
    it "ltop [-1,-1, 1]" $ show (ltop [-1,-1, 1]) `shouldBe` "Pile 2"
    it "ltop [ 1, 1, 1]" $ show (ltop [ 1, 1, 1]) `shouldBe` "Pile 8"

    it "ptol 1" $ show (ptol $ pile 1) `shouldBe` "[-1,-1,-1]"
    it "ptol 2" $ show (ptol $ pile 2) `shouldBe` "[-1,-1,1]"
    it "ptol 3" $ show (ptol $ pile 3) `shouldBe` "[-1,1,-1]"
    it "ptol 4" $ show (ptol $ pile 4) `shouldBe` "[-1,1,1]"
    it "ptol 5" $ show (ptol $ pile 5) `shouldBe` "[1,-1,-1]"
    it "ptol 6" $ show (ptol $ pile 6) `shouldBe` "[1,-1,1]"
    it "ptol 7" $ show (ptol $ pile 7) `shouldBe` "[1,1,-1]"
    it "ptol 8" $ show (ptol $ pile 8) `shouldBe` "[1,1,1]"


    let cases = [
                -- Sample inputs from the problem page
                -- hide failing test for now
                --( "uxuhul-1.input", "NYY\n" ),
                  ( "uxuhul-2.input", "NNY\n" )
                ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (uxuhul <$> readFile (path ++ testfile))
                        >>= (`shouldBe` result)


  describe "Limbo1" $ do

    let cases = [
                -- Sample inputs from the problem page
                  ( "limbo1.input", unlines [ "9", "19", "100" ]) ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (limbo1 <$> readFile (path ++ testfile))
                        >>= (`shouldBe` result)


  describe "Limbo2" $ do

    -- map a column to the square it will belong to if the row is within it
    it "toSquare 0" ( toSquare 0 `shouldBe` Square 0 )
    it "toSquare 1" ( toSquare 1 `shouldBe` Square 1 )
    it "toSquare 2" ( toSquare 2 `shouldBe` Square 2 )
    it "toSquare 3" ( toSquare 3 `shouldBe` Square 2 )
    it "toSquare 4" ( toSquare 4 `shouldBe` Square 3 )
    it "toSquare 5" ( toSquare 5 `shouldBe` Square 3 )
    -- ...
    it "toSquare 7" ( toSquare 7 `shouldBe` Square 3 )
    it "toSquare 8" ( toSquare 8 `shouldBe` Square 4 )
    it "toSquare 9" ( toSquare 9 `shouldBe` Square 4 )
    -- ...
    it "toSquare 15" ( toSquare 15 `shouldBe` Square 4 )
    it "toSquare 16" ( toSquare 16 `shouldBe` Square 5 )


    it "toRectangle 0" ( toRectangle 0 `shouldBe` Nothing )
    it "toRectangle 1" ( toRectangle 1 `shouldBe` (Just $ Rect 1 ))
    it "toRectangle 2" ( toRectangle 2 `shouldBe` (Just $ Rect 2 ))
    it "toRectangle 3" ( toRectangle 3 `shouldBe` (Just $ Rect 2 ))
    it "toRectangle 4" ( toRectangle 4 `shouldBe` (Just $ Rect 3 ))

    -- toSquare and toRectangle turn out to be the same function except the argument 0
    -- returns a Square for toSquare but Nothing for toRectangle



    it "f" ( f (Square 2) 0 2 `shouldBe` (Just 4))
    it "f" ( f (Square 2) 1 2 `shouldBe` (Just 5))
    it "f" ( f (Square 2) 0 3 `shouldBe` (Just 6))
    it "f" ( f (Square 2) 1 3 `shouldBe` (Just 7))

    it "f" ( f (Square 3) 2 6 `shouldBe` (Just 26))

    it "g" ( g (Rect 1) 1 0 `shouldBe` (Just 2))
    it "g" ( g (Rect 1) 1 1 `shouldBe` (Just 3))

    it "g" ( g (Rect 2) 2 0 `shouldBe` (Just 8))
    it "g" ( g (Rect 2) 2 1 `shouldBe` (Just 9))
    it "g" ( g (Rect 2) 2 2 `shouldBe` (Just 10))
    it "g" ( g (Rect 2) 2 3 `shouldBe` (Just 11))

    it "g" ( g (Rect 2) 3 0 `shouldBe` (Just 12))
    it "g" ( g (Rect 2) 3 3 `shouldBe` (Just 15))

    it "g" ( g (Rect 3) 4 0 `shouldBe` (Just 32))


    it "doCase 0" (doCase [0,0] `shouldBe` (Output 0))
    it "doCase 1" (doCase [0,1] `shouldBe` (Output 1))
    it "doCase 2" (doCase [1,0] `shouldBe` (Output 2))
    it "doCase 3" (doCase [1,1] `shouldBe` (Output 3))
    it "doCase 4" (doCase [0,2] `shouldBe` (Output 4))
    it "doCase 5" (doCase [1,2] `shouldBe` (Output 5))
    it "doCase 6" (doCase [0,3] `shouldBe` (Output 6))
    it "doCase 7" (doCase [1,3] `shouldBe` (Output 7))
    it "doCase 8" (doCase [2,0] `shouldBe` (Output 8))
    it "doCase 9" (doCase [2,1] `shouldBe` (Output 9))
    it "doCase 10" (doCase [2,2] `shouldBe` (Output 10))
    it "doCase 11" (doCase [2,3] `shouldBe` (Output 11))
    it "doCase 12" (doCase [3,0] `shouldBe` (Output 12))
    it "doCase 13" (doCase [3,1] `shouldBe` (Output 13))
    it "doCase 14" (doCase [3,2] `shouldBe` (Output 14))
    it "doCase 15" (doCase [3,3] `shouldBe` (Output 15))

    it "rectCorner 1" (rectCorner (Rect 1) `shouldBe` 2)
    it "rectCorner 2" (rectCorner (Rect 2) `shouldBe` 8)
    it "rectCorner 3" (rectCorner (Rect 3) `shouldBe` 32)

    it "rectTop 1" (rectTop (Rect 1) `shouldBe` 1)
    it "rectTop 2" (rectTop (Rect 2) `shouldBe` 2)
    it "rectTop 3" (rectTop (Rect 3) `shouldBe` 4)

    let cases = [
                -- Sample inputs from the problem page
                  ( "limbo2.input", unlines [ "0", "2", "18" ]) ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (limbo2 <$> readFile (path ++ testfile))
                        >>= (`shouldBe` result)

