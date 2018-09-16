module Huffman where

import Data.List

data HuffmanTree a = HuffmanNode a Int (HuffmanTree a) (HuffmanTree a) | Nil deriving (Show)

huffmanLeaf :: a -> Int -> HuffmanTree a
huffmanLeaf a num = HuffmanNode a num Nil Nil

reps :: (Eq a) => a -> [a] -> Int
reps _ [] = 0
reps y (x:xs) = (if y == x then 1 else 0) + reps y xs

qty :: (Eq a) => [a] -> [HuffmanTree a]
qty l = map (\x -> huffmanLeaf x (reps x l)) (nub l)

addToQueue :: HuffmanTree -> [HuffmanTree] -> [HuffmanTree]
addToQueue t [] = [t]
addToQueue node@(HuffmanNode _ new _ _) ((HuffmanNode _ current _ _):xs)
    | new < current = node ++ 