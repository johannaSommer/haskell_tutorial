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



