module Problem10Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Problem10

spec1 :: Spec
spec1 = do
  describe "Problem10.myLast" $ do
    it "returns the last element of a list" $ do
      myLast [1,2,3,4] `shouldBe` (4 :: Int)
      myLast ['x','y','z'] `shouldBe` ('z' :: Char)

    it "throws an exception if used with an empty list" $ do
      evaluate (myLast []) `shouldThrow` errorCall "empty list!"

spec2 :: Spec
spec2 = do
  describe "Problem10.myButLast" $ do
    it "returns the last but one element of a list" $ do
      myButLast [1,2,3,4] `shouldBe` (3 :: Int)
      myButLast ['a'..'z'] `shouldBe` ('y' :: Char)

    it "throws an exception if used with an empty list" $ do
      evaluate (myButLast []) `shouldThrow` errorCall "empty list!"

spec3 :: Spec
spec3 = do
  describe "Problem10.elementAt" $ do
    it "returns the K'th element of a list. The first element in the list is number 1" $ do
      elementAt [1,2,3,4] 2 `shouldBe` (2 :: Int)
      elementAt "haskell" 5 `shouldBe` ('e' :: Char)

    it "throws an exception if used with an empty list" $ do
      evaluate (elementAt [] 1) `shouldThrow` errorCall "empty list!"

    it "throws an exception if index is out of bounds" $ do
      evaluate (elementAt [1,2,3] 4) `shouldThrow` errorCall "index out of bounds!"  

spec4 :: Spec
spec4 = do
  describe "Problem10.myLength" $ do
    it "returns the number of elements of a list" $ do
      myLength [123, 456, 789] `shouldBe` (3 :: Int)
      myLength "Hello, world!" `shouldBe` (13 :: Int)
      myLength [] `shouldBe` (0 :: Int)

spec5 :: Spec
spec5 = do
  describe "Problem10.myReverse" $ do
    it "returns a reversed list" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1]

spec6 :: Spec
spec6 = do
  describe "Problem10.isPalindrome" $ do
    it "Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x)" $ do
      isPalindrome [1,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True

spec7 :: Spec
spec7 = do
  describe "Problem10.myFlatten" $ do
    it "Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively)." $ do
      myFlatten (Elem 5) `shouldBe` [5]
      myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
      myFlatten (List [] :: NestedList Int) `shouldBe` ([] :: [Int])