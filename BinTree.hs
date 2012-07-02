module BinTree where

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
               deriving (Show, Eq)

reverse :: BinTree a -> BinTree a
reverse (Leaf a)   = Leaf a
reverse (Node x y) = Node (BinTree.reverse y) (BinTree.reverse x)
