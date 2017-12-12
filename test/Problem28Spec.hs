module Problem28Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Problem28

spec21 :: Spec
spec21 = do
  describe "Problem28.insertAt" $ do
    it "Insert an element at a given position into a list." $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
    it "Insert an element at beginning of list." $ do  
      insertAt 'X' "abcd" 1 `shouldBe` "Xabcd"
    it "Insert an element at end of list." $ do  
      insertAt 'X' "abcd" 4 `shouldBe` "abcXd"
    it "Insert an element at a given position small than or equal to 0." $ do  
      evaluate(insertAt 'X' "abcd" 0) `shouldThrow` errorCall "insertAt index <= 0"
    it "Insert an element at a given position larger than length of list." $ do
      evaluate(insertAt 'X' "abcd" 5) `shouldThrow` errorCall "insertAt index larger than length of list"

spec22 :: Spec
spec22 = do
  describe "Problem28.range" $ do
    it "Create a list containing all integers within a given range." $ do
      range 4 9 `shouldBe` [4,5,6,7,8,9]

spec28a :: Spec
spec28a = do
  describe "Problem28.lsort" $ do
    it "Sorts a list of lists according to length of sublists" $ do
      lsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["o","de","de","mn","abc","fgh","ijkl"]

spec28b :: Spec
spec28b = do
  describe "Problem28.lfsort" $ do
    it "Sorts a list of lists according to length frequency of sublists" $ do
      lfsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["ijkl","o","abc","fgh","de","de","mn"]
      