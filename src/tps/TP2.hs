module TP2 where

data Bit = One | Zero deriving (Show, Eq)
type Binary = [Bit]

data SumResult = SumResult{carry::Bit, result::Bit} deriving Show

bitAdd :: Bit -> Bit -> SumResult
bitAdd Zero Zero = SumResult Zero Zero
bitAdd One One = SumResult One Zero
bitAdd _ _ = SumResult Zero One

fullBitAdd :: Bit -> Bit -> Bit -> Binary
fullBitAdd x y z = [result carryAdd, result secondAdd]
    where 
        firstAdd = bitAdd x y
        secondAdd = bitAdd (result firstAdd) z
        carryAdd = bitAdd (carry firstAdd) (carry secondAdd)

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


data GNode a = GNode a [GNode a]
type Graph a = [GNode a]



