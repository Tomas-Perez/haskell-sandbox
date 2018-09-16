module TP1 where


reverse2 :: [a] -> [a]
reverse2 list = reverse2' list []

reverse2' :: [a] -> [a] -> [a]
reverse2' [] carry = carry
reverse2' (x:xs) carry = reverse2' xs (x:carry)


sumAll :: [Int] -> Int
sumAll list = sumAll' list 0

sumAll' :: [Int] -> Int -> Int
sumAll' [] carry = carry
sumAll' (x:xs) carry = sumAll' xs (carry+x)

palindrome :: Eq a => [a] -> Bool
palindrome x = x == reverse2 x

worsePalindrome :: Eq a => [a] -> Bool
worsePalindrome [] = True
worsePalindrome [x] = True
worsePalindrome (x:xs)
    | x == last xs = worsePalindrome (init xs)
    | otherwise = False

repCount :: Eq a => [a] -> a -> Int
repCount [] _ = 0
repCount (x:xs) a = (if x == a then 1 else 0) + repCount xs a 


fac 0 = 1
fac n = n * fac (n-1)