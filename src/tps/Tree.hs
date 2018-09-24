module Tree where

data Tree a = Node a (Tree a) (Tree a) | Nil deriving Show

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Nil = Node x Nil Nil
insert x node@(Node a l r)
        | a == x = node
        | x > a = Node a l (insert x r)
        | x < a = Node a (insert x l) r

inOrder :: (Show a) => Tree a -> String
inOrder Nil = ""
inOrder (Node a l r) = inOrder l ++ show a ++ inOrder r

depth :: Tree a -> Int
depth t = depth' t 0

depth' :: Tree a -> Int -> Int
depth' Nil current = current
depth' (Node _ left right) current = max (depth' left (current + 1)) (depth' right (current + 1))