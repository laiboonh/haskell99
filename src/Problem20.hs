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
repli (x:xs) times = (take times $ repeat x) ++ repli xs times 

dropEvery :: [a] -> Int -> [a]
dropEvery xs target = foldr (\tuple acc -> if (snd tuple) /= 3 then (fst tuple) : acc else acc) [] tuples 
  where tuples = zip xs $ cycle [1..target]

split :: [a] -> Int -> ([a], [a])
split [] index | index > 0 = error "list length less than split point"
               | index <= 0 = ([],[])
split lst@(x:xs) index | index > 0 = let result = split xs (index-1)
                                     in (x:fst result, snd result)
                       | index <=0 = ([],lst)

slice :: [a] -> Int -> Int -> [a]
slice lst srt end | srt > end = error "start index bigger than end index"
                  | otherwise = headN (trimHead lst (srt-1)) (end-srt+1)

trimHead :: [a] -> Int -> [a]
trimHead [] index | index > 0 = error "trimHead : trying to trim an empty list"  
                  | index <= 0 = []
trimHead lst@(x:xs) index | index > 0 = trimHead xs (index-1)
                      | index <= 0 = lst  

headN :: [a] -> Int -> [a]
headN [] n | n > 0 = error "headH: trying to get first N elements from an empty list"    
           | n <= 0 = []
headN (x:xs) n | n > 0 = x : headN xs (n-1)
               | n <= 0 = []           
           
rotate :: [a] -> Int -> [a]
rotate [] n | n == 0 = []
            | otherwise = error "rotating an empty list"
rotate lst@(x:xs) n | n == 0 = lst
                    | n > 0 = rotate (xs ++ [x]) (n-1)
                    | n < 0 = rotate lst (n + length lst)
                       
removeAt :: Int -> [a] -> (a,[a])
removeAt _ [] = error "cannot removeAt an empty list"
removeAt n xs = removeAt' [] n xs
  where removeAt' _ _ [] = error "index out of bounds"
        removeAt' acc 0 xs = error "removeAt 0 xs"
        removeAt' acc 1 (x:xs) = (x,acc ++ xs)
        removeAt' acc idx (x:xs) = removeAt' (x:acc) (idx-1) xs
        