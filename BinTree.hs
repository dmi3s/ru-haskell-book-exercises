module BinTree where

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
      deriving (Show, Eq)
