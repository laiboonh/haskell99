module Problem10 where

myLast :: [a] -> a
myLast [] = error "empty list!"
myLast [x] = x
myLast (_ : xs) = myLast xs 

myButLast :: [a] -> a
myButLast [] = error "empty list!"
myButLast [x] = error "one element list!"
myButLast (x:xs) | length xs == 1 = x
                 | otherwise = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] index = error "empty list!"
elementAt xs index | index > length xs  = error "index out of bounds!"
elementAt (x:_) 1 = x
elementAt (_:xs) index = elementAt xs (index-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse list = myReverse' list []
  where myReverse' [] reversed = reversed
        myReverse' (x:xs) reversed = myReverse' xs (x:reversed)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs

data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x1:x2:xs) | x1 == x2 = compress (x1:xs)
                    | otherwise = x1:(compress (x2:xs)) 

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head packed)
            then (x:(head packed)) : (tail packed)
            else [x] : packed   
  where packed = pack xs  

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map countElem $ pack xs 
  where countElem elems = (length elems, head elems)