module Nat where

data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)


instance Num Nat where
  (+) a Zero     = a
  (+) Zero a     = a
  (+) (Succ a) (Succ b) = Succ( Succ (a+b))
  negate _       = error "negate is undefined for Nat"
  (*) a Zero     = Zero
  (*) a (Succ b) = a + (a * b)
  abs x          = x
  signum Zero    = Zero
  signum _       = Succ Zero
  fromInteger 0  = Zero
  fromInteger n  = Succ (fromInteger (n-1))


beside2 :: Nat -> Nat -> Bool
beside2 (Succ x) (Succ y) = beside2 x y
beside2 Zero x            = beside2 x Zero
beside2 x Zero            = x == 3


pow :: Nat -> Nat -> Nat
pow a Zero = 1
pow Zero a = Zero
pow a (Succ b) = a * pow a b
