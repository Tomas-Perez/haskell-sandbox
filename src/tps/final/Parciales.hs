module Parciales where

data Tree a = Node a (Tree a) (Tree a) | Nil

instance (Eq a) => Eq (Tree a) where
    Nil == Nil = True
    (Node a1 l1 r1) == (Node a2 l2 r2) = a1 == a2 && l1 == l2 && r1 == r2
    _ == _ = False

instance (Show a) => Show (Tree a) where
    show Nil = ""
    show (Node a Nil Nil) = show a
    show (Node a l Nil) = show l ++ ", " ++ show a
    show (Node a Nil r) = show a ++ ", " ++ show r
    show (Node a l r) = show l ++ ", " ++ show a ++ ", " ++ show r

type Graph = [(Int, [Int])]

hasCycles :: Graph -> Bool
hasCycles graph = any (\x -> hasCycles'' x graph) $ map fst graph

hasCycles'' :: Int -> Graph -> Bool
hasCycles'' start graph = hasCycles' graph start [] (getNeighbors start graph)

hasCycles' :: Graph -> Int -> [Int] -> [Int] -> Bool
hasCycles' _ _ _ [] = False
hasCycles' graph current visited neighbors = current `elem` visited || any (\x -> hasCycles' graph x (current:visited) (getNeighbors x graph)) neighbors
        
getNeighbors :: Int -> Graph -> [Int]
getNeighbors a [] = error $ "Node " ++ show a ++ " does not exist"
getNeighbors a (n:ns) = if a == fst n then snd n else getNeighbors a ns

a :: Graph
a = [(4, [5]), (5, [1]),(1, [2]), (2, [3]), (3, [4])]

b :: Graph
b = [(1, [1])]

c :: Graph
c = [(4, []), (5, [1]),(1, [2]), (2, [3]), (3, [4])]

data Bit = Zero | One deriving Show
type Binary = [Bit]

toBinary :: Int -> Binary
toBinary 0 = [Zero]
toBinary 1 = [One]
toBinary n = toBinary (div n 2) ++ toBinary (rem n 2)

elemByBinary :: Tree a -> Binary -> Maybe a
elemByBinary (Node a _ _) [] = Just a
elemByBinary Nil _ = Nothing
elemByBinary (Node _ l _) (One:bs) = elemByBinary l bs
elemByBinary (Node _ _ r) (Zero:bs) = elemByBinary r bs