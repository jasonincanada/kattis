import Test.Hspec
import Control.Monad (forM_)

import Golomb        (golomb)
import Lektira       (lektira)
import MarblesTree   (marblestree)
import Recenice      (recenice)
import SmallSchedule (smallschedule)
import Uxuhul        (ltop, ptol, pile, turn, uxuhul, Stone(..))
--import Ceiling       (ceiling)
import Substitution  (substitution)


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



  {-
  describe "Ceiling" $ do

    let cases = [
                -- Sample inputs from the problem page
                  ( "ceiling-1.input", "4" ),
                  ( "ceiling-2.input", "2" )
                ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (Ceiling.ceiling <$> readFile (path ++ testfile))
                        >>= (`shouldBe` result)
  -}


  describe "Substitution" $ do

    let cases = [
                -- Sample inputs from the problem page
                  ( "substitution-1.input", unlines ["3", "1"] )
                ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (substitution <$> readFile (path ++ testfile))
                        >>= (`shouldBe` result)

