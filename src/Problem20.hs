module Problem20 where

import Problem10

data Elem a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Elem a]
encodeModified xs = map convert $ encode xs
  where convert (1, elem) = Single elem
        convert (times, elem) = Multiple times elem

decodeModified :: Eq a => [Elem a] -> [a]
decodeModified elems = foldr convertBack [] elems
  where convertBack (Single a) acc =  a : acc
        convertBack (Multiple times a) acc = (take times $ repeat a) ++ acc

encodeDirect :: Eq a => [a] -> [Elem a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) = same packed x $ head packed
  where packed = encodeDirect xs 
        same acc x (Single y) = if x == y then (Multiple 2 x) : (tail acc) else (Single x) : acc
        same acc x (Multiple idx y) = if x == y then (Multiple (idx+1) x) : (tail acc) else (Single x) : acc

dupli :: [a] -> [a]
dupli xs = foldr (\elem acc -> elem : elem : acc) [] xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) times = take times $ repeat x ++ repli xs times 