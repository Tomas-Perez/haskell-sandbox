module Huffman where

import Data.List
import Binary
import qualified Data.Map as Map
import Data.Map (Map)

data HuffmanTree a = HuffmanNode (Maybe a) Int (HuffmanTree a) (HuffmanTree a) | Nil deriving (Show)

instance Eq (HuffmanTree a) where
    (HuffmanNode _ a _ _) == (HuffmanNode _ b _ _) = a == b
    Nil == Nil = True
    _ == _ = False

instance Ord (HuffmanTree a) where
    compare (HuffmanNode _ a _ _) (HuffmanNode _ b _ _) = compare a b
    compare Nil Nil = EQ
    compare t Nil = GT
    compare Nil t = LT

huffmanLeaf :: a -> Int -> HuffmanTree a
huffmanLeaf a num = HuffmanNode (Just a) num Nil Nil

combineNodes :: HuffmanTree a -> HuffmanTree a -> HuffmanTree a
combineNodes left@(HuffmanNode _ a _ _) right@(HuffmanNode _ b _ _) = HuffmanNode Nothing (a+b) left right

reps :: (Eq a) => a -> [a] -> Int
reps _ [] = 0
reps y (x:xs) = (if y == x then 1 else 0) + reps y xs

qty :: (Eq a) => [a] -> [HuffmanTree a]
qty l = map (\x -> huffmanLeaf x (reps x l)) (nub l)

addToQueue :: HuffmanTree a -> [HuffmanTree a] -> [HuffmanTree a]
addToQueue t [] = [t]
addToQueue node list@(x:xs)
    | node <= x = node : list
    | node > x = x : (addToQueue node xs)

listToQueue :: [HuffmanTree a] -> [HuffmanTree a]
listToQueue a = listToQueue' a []

listToQueue' :: [HuffmanTree a] -> [HuffmanTree a] -> [HuffmanTree a]
listToQueue' [] carry = carry
listToQueue' (x:xs) carry = listToQueue' xs (addToQueue x carry)

queueToTree :: [HuffmanTree a] -> Maybe (HuffmanTree a)
queueToTree a = case resultList of
    [] -> Nothing
    otherwise -> Just (head resultList)
    where resultList = queueToTree' a

queueToTree' :: [HuffmanTree a] -> [HuffmanTree a]
queueToTree' (x:y:xs) = queueToTree' (addToQueue (combineNodes x y) xs)
queueToTree' a = a

treeToCodes' :: HuffmanTree a -> Binary -> [(a, Binary)]
treeToCodes' (HuffmanNode (Just a) _ left right) [] = [(a, [One])]
treeToCodes' Nil _ = []
treeToCodes' (HuffmanNode val _ left right) code = case val of
    Just a -> (a, reverse code) : treeToCodes' left (Zero:code) ++ treeToCodes' right (One:code)
    Nothing -> treeToCodes' left (Zero:code) ++ treeToCodes' right (One:code)


treeToCodes :: Ord a => HuffmanTree a -> Map a Binary
treeToCodes tree = Map.fromList(treeToCodes' tree [])

huffmanTable :: Ord a => [a] -> Maybe (Map a Binary)
huffmanTable list = fmap treeToCodes (queueToTree $ listToQueue $ qty list) 

encode :: (Ord a) => [a] -> Map a Binary -> [Binary]
encode a mapper = map ((Map.!) mapper) a