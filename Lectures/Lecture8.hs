module Lecture8 where

import Data.Ratio
import Test.QuickCheck
import Data.List
import Data.Function


data Fraction = Over Integer Integer
instance Num Fraction where
 (Over a1 b1) + (Over a2 b2) = norm $ Over (a1*b2 + a2*b1) (b1 * b2)
 (Over a1 b1) - (Over a2 b2) = norm $ Over (a1*b2 - a2*b1) (b1 * b2)
 (Over a1 b1) * (Over a2 b2) = norm $ Over (a1 * a2) (b1 * b2)
 negate (Over a b) = Over (-a) b
 fromInteger a = Over a 1
 abs (Over a b) = Over (abs a) (abs b)
 signum (Over a b) = Over (signum a * signum b) 1

norm :: Fraction -> Fraction
norm (Over a b) = (a `div` c) `Over` (b `div` c)
  where c = gcd a b


data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

--- skip, already in repetitorium exercises

f1 = map (+1)
f2 = map (*2) .  map (+1)
f3 = filter (>1) . map (+1)
f4 = foldr (\ f acc -> f acc ) 0 . map ($ 5)
f5 f g = f . g
f6 f (x , y ) = uncurry

f7' f x = flip f
f7'' f = const (flip f)
f7''' = const . flip

data Tree a = Leaf | Node ( Tree a ) a ( Tree a )

mirror :: Eq a => Tree a -> Tree a
mirror Leaf = Leaf
mirror (Node l a r) = (Node (mirror r) a (mirror l))

symmetric :: Eq a => Tree a -> Bool
symmetric Leaf = True
symmetric (Node l _ r) = mirror l == r=

sorted :: Ord a => [a] -> Bool
sorted (x1:x2:xs) = x1 < x2 && sorted (x2:xs)
sorted _ = True

isBST :: Ord a => Tree a -> Bool
isBST = sorted . inorder

data Poly a = Poly [(a, Integer)]
  deriving (Eq)

-- takes the derivative of a polynomial
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Poly ps) = Poly [(fromInteger e * c, e - 1) | (c, e) <- ps, e /= 0]

-- addition of two polynomials
polyAdd :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
polyAdd (Poly []) q = q
polyAdd p (Poly []) = p
polyAdd p@(Poly ((c1, e1) : ps)) q@(Poly ((c2, e2) : qs))
  | e1 < e2      = let Poly rs = polyAdd (Poly ps) q in
                   Poly ((c1, e1) : rs)
  | e1 > e2      = let Poly rs = polyAdd p (Poly qs) in
                   Poly ((c2, e2) : rs)
  | c1 + c2 == 0 = polyAdd (Poly ps) (Poly qs)
  | otherwise    = let Poly rs = polyAdd (Poly ps) (Poly qs) in
                   Poly ((c1 + c2, e1) : rs)

polyShift :: (Eq a, Num a) => (a, Integer) -> Poly a -> Poly a
polyShift _ [] = Poly []
polyShift (c, e) (Poly ps) = Poly [(c * c', e + e') | (c', e') <- ps]

instance (Eq a, Num a) => Num (Poly a) where
    (+) = polyAdd
    negate = polyShift (-1, 0)
    (-) = flip $ (+).negate(-)



