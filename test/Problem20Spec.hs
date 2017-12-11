module Problem20Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Problem20

spec11 :: Spec
spec11 = do
  describe "Problem20.encodeModified" $ do
    it "Encode but only elements with duplicates are transferred as (N E) lists." $ do
      encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

spec12 :: Spec
spec12 = do
  describe "Problem20.decodeModified" $ do
    it "Given a run-length code list generated as specified in problem 11. Construct its uncompressed version." $ do    
      decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"

spec13 :: Spec
spec13 = do
  describe "Problem20.encodeDirect" $ do
    it "Implement the so-called run-length encoding data compression method directly" $ do
      encodeDirect "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

spec14 :: Spec
spec14 = do
  describe "Problem20.dupli" $ do
    it "Duplicate the elements of a list." $ do
      dupli [1, 2, 3] `shouldBe` [1,1,2,2,3,3]

spec15 :: Spec
spec15 = do
  describe "Problem20.repli" $ do
    it "Replicate the elements of a list a given number of times." $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"

spec16 :: Spec
spec16 = do
  describe "Problem20.dropEvery" $ do
    it "Drop every N'th element from a list." $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

spec17 :: Spec
spec17 = do
  describe "Problem20.split" $ do
    it "Split a list into two parts; the length of the first part is given." $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

spec18 :: Spec
spec18 = do
  describe "Problem20.slice" $ do
    it "Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1." $ do
      trimHead ['a','b','c','d','e','f','g','h','i','k'] 3 `shouldBe` "defghik"
      headN ['a','b','c','d','e','f','g','h','i','k'] 4 `shouldBe` "abcd"
      slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

spec19 :: Spec
spec19 = do
  describe "Problem20.rotate" $ do
    it "Rotate a list N places to the left." $ do
      rotate ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
      rotate ['a','b','c','d','e','f','g','h'] (-2) `shouldBe` "ghabcdef"

spec20 :: Spec
spec20 = do
  describe "Problem20.removeAt" $ do
    it "Remove the K'th element from a list." $ do
      removeAt 2 "abcd" `shouldBe` ('b',"acd")
