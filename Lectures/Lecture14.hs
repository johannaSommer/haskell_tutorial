{-# LANGUAGE DeriveFunctor #-}

module Lecture14 where


data Tree a = Node a (Tree a) (Tree a)
  deriving (Functor)


-- Deriving Functor for Tree eliminates boilerplate.
-- The derived instance is equivalent to:
-- instance Functor Tree where
--   fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

nats :: Tree Integer
nats = go 0 1 where
  go n step = Node n (go l step') (go r step')
    where
      l = n + step
      r = l + step
      step' = step * 2

(!!!) :: Tree a -> Integer -> a
Node m _ _ !!! 0 = m
Node _ l r !!! n = case (n - 1) `divMod` 2 of
    (q,0) -> l !!! q
    (q,1) -> r !!! q

collatz_steps_list :: [Integer]
collatz_steps_list = [t !!! i | i <- [0..]]
  where
    t = fmap collatz_steps nats

    collatz_steps 0 = 0
    collatz_steps 1 = 0
    collatz_steps n
      | n `mod` 2 == 0 = 1 + t !!! (n `div` 2)
      | otherwise = 1 + t !!! (3 * n + 1)