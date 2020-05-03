import Test.Hspec
import Control.Monad (forM_)

import Golomb        (golomb)
import Lektira       (lektira)
import MarblesTree   (marblestree)
import Recenice      (recenice)
import SmallSchedule (smallschedule)

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


