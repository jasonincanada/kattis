import Test.Hspec
import Control.Monad (forM_)

import MarblesTree (marblestree)

path = "test/inputs/"

main :: IO ()
main = hspec $ do

  describe "MarblesTree" $ do

    let cases = [ ( "marblestree-1.input", 7  )
                , ( "marblestree-2.input", 14 )
                , ( "marblestree-3.input", 20 )
                ]

    forM_ cases $
      \(testfile, result) ->
        it testfile $ (marblestree <$> readFile (path ++ testfile))
                        >>= (`shouldBe` (unlines [show result]))

