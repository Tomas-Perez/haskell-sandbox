module Huffman where

data HTree = Leaf Char | Branch HTree HTree

sortByFreq :: [(Int, HTree)] -> [(Int, HTree)]
sortByFreq [] = []
sortByFreq (x:xs) = [y | y <- xs, fst y < fst x] ++ [x] ++ [z | z <- xs, fst z <= fst x]

insertToQueue :: (Int, HTree) -> [(Int, HTree)] -> [(Int, HTree)]
insertToQueue a [] = [a]
insertToQueue a@(fr2, _) (x@(fr1, _):xs)
    | fr2 > fr1 = a : (x:xs)
    | otherwise = x : insertToQueue a xs

repsToTable :: [(Char, Int)] -> HuffmanTable
repsToTable freqs = makeTable $ makeTree $ sortByFreq [(fr, Leaf c) | (c, fr) <- freqs]
    where 
        makeTree [(_, t)] = t
        makeTree ((fr1, t1) : (fr2, t2) : ns) = makeTree $ insertToQueue (fr1 + fr2, Branch t1 t2) ns
        makeTable (Leaf c) = [(c, "")]
        makeTable (Branch l r) = [(x, '0':code) | (x, code) <- makeTable l] ++ [(x, '1':code) | (x, code) <- makeTable r]

type HuffmanTable = [(Char, String)]

compress :: HuffmanTable -> String -> Maybe [String]
compress _ [] = Just []
compress t (c:cs) = 
    do
        x <- find t c
        xs <- compress t cs
        return (x:xs)

find :: HuffmanTable -> Char -> Maybe String
find [] key = Nothing
find ((c, code) : ts) key
    | c == key = Just code
    | otherwise = find ts key