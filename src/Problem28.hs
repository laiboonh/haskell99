module Problem28 where

import System.Random
import Control.Monad (replicateM)
import Problem20
import Data.List (sort, sortOn, groupBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n | n == 1 = x:xs 
                | n <=0 = error "insertAt index <= 0"
                | n > length xs = error "insertAt index larger than length of list"
                | otherwise = foldr fn [] xs
  where fn elem acc | length acc == moment = elem : x : acc
                    | otherwise = elem : acc
        moment = length xs - n + 1

range :: (Ord a, Num a) => a -> a -> [a]
range x y | x <= y = x : range (x+1) y
          | otherwise = []

rnd_select :: [a] -> Int -> IO [a]          
rnd_select xs n | n < 0 = error "N must be greater than zero."
                | otherwise = replicateM n rand
                  where rand = do r <- randomRIO (0, (length xs) - 1)
                                  return (xs!!r)

diff_select :: Int -> Int -> IO [Int]
diff_select n max | n == 0 = return []
                  | n < 0 = error "n cannot be less than 0"
                  | otherwise = diff_select' n [1..max]

diff_select' :: Eq a => Int -> [a] -> IO [a]
diff_select' 0 _ = return []
diff_select' _ [] = error "empty list"
diff_select' n xs = do
                rndIdx <- randomRIO(0, length xs - 1)
                let remaining = (take rndIdx xs) ++ (drop (rndIdx+1) xs)
                pure ((xs !! rndIdx) :) <*> diff_select' (n-1) remaining

rnd_permu :: Eq a => [a] -> IO [a]
rnd_permu xs = diff_select' (length xs) xs 

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] 
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

lsort :: [[a]] -> [[a]]
lsort xs = sortOn sortFn xs
  where sortFn elem = length elem

lfsort :: Ord a => [[a]] -> [[a]]
lfsort xs = Map.foldr foldFn [] freqMap
        where freqMap = lFreqMap xs
              foldFn value acc = (sort value) ++ acc  

lFreqMap :: [[a]] -> Map.Map Int [[a]]
lFreqMap xs = foldr foldFn Map.empty lst
        where foldFn elem acc = if isNothing (Map.lookup freq acc) 
                                then Map.insert freq elem acc
                                else Map.adjust (\elems -> elem ++ elems) freq acc
                                where freq = length elem                  
              lst = groupBy (\x y -> length x == length y) $ lsort xs