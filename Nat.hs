module Nat where

data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)


instance Num Nat where
  (+) a Zero     = a
  (+) a (Succ b) = Succ (a+b)
  negate _       = error "negate is udefined for Nat"
  (*) a Zero     = Zero
  (*) a (Succ b) = a + (a * b)
  abs x          = x
  signum Zero    = Zero
  signum _       = Succ Zero
  fromInteger 0  = Zero
  fromInteger n  = Succ (fromInteger (n-1))


beside :: Nat -> Nat -> Bool
beside (Succ x) (Succ y) = beside x y
beside Zero x            = x == 1
beside x Zero            = x == 1
