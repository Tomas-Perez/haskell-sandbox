module ListUtil where

showList' :: Show a => [a] -> Int -> String
showList' [] _ = ""
showList' (x:xs) n = show n ++ "\t" ++ show x ++ "\n" ++ showList' xs (n+1)

indexOf :: Eq a =>  a -> [a] -> Int -> Int
indexOf e [] _ = -1
indexOf e (x:xs) n
    | x == e = n
    | otherwise = indexOf e xs (n+1)

putIn :: a -> Int -> [a] -> Int -> [a]
putIn e _ [] _ = e:[]
putIn e i (x:xs) n
    | i == n = e : xs
    | otherwise = x : putIn e i xs (n+1)

remove :: Int -> [a] -> Int -> [a]
remove _ [] _ = []
remove i (x:xs) n
    | i == n = xs
    | otherwise = x : remove i xs (n+1)

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = lowerSorted ++ (x : higherSorted)
    where lowerSorted = sort [a | a <- xs, a <= x]
          higherSorted = sort [a | a <- xs, a > x]