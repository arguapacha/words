import Test.Hspec
import Lib
import Data

gwc = gridWithCoords grid

testFindWord word = 
  let (Just result) = findWord gwc word
      string = map cell2char result
  in string `shouldBe` word

main :: IO ()
main = hspec $ do
        describe "formatGrid" $ do
          it "Should concatenate lines with newline" $ do
            (formatGrid (gridWithCoords ["abc","def"])) `shouldBe` "abc\ndef\n"

        describe "findWord" $ do
          it "Should be able to find a word" $ do 
            testFindWord "HASKELL" 
            testFindWord "LISP"

          it "Should not find a non-existent word" $ do
            findWord gwc "PERL" `shouldBe` Nothing


        describe "findWords" $ do
          it "Should find all languages" $ do
            let found = findWords gwc languages 
                asString = map (map cell2char) found
            asString `shouldBe` languages
          it "Should not find other languages" $ do
            findWords gwc ["SPANISH", "ENGLISH"] `shouldBe` []
