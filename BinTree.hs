module BinTree where
import Nat

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
               deriving (Show, Eq)

reverse :: BinTree a -> BinTree a
reverse (Leaf e)   = Leaf e
reverse (Node x y) = Node (BinTree.reverse y) (BinTree.reverse x)


depth :: BinTree a -> Nat
depth (Leaf e)   = Zero
depth (Node x y) = Succ (max (depth x) (depth y))
