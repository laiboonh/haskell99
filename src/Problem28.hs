module Problem28 where

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n | n == 1 = x:xs 
                | n <=0 = error "insertAt index <= 0"
                | n > length xs = error "insertAt index larger than length of list"
                | otherwise = foldr fn [] xs
  where fn elem acc | length acc == moment = elem : x : acc
                    | otherwise = elem : acc
        moment = length xs - n + 1 