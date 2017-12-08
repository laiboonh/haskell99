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