module Lecture7 where

import Data.Char

iterWhile :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterWhile test f x = if test x (f x) then iterWhile test f (f x) else x

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint = iterWhile (/=)

mySqrt :: Double -> Double
mySqrt n = fixpoint (\x -> (x + n/x) / 2) 1

findSup :: Ord a => (a -> a) -> a -> a -> a
findSup f m = iterWhile (const (<=m)) f

mapState :: (s -> a -> (b,s)) -> s -> [a] -> ([b],s)
mapState f s [] = ([], s)
mapState f s (x:xs) = (x':xs', s'')
  where
    (x', s') = f s x
    (xs', s'') = mapState f s' xs


inc s x = (( s , x ) , s + 1)

f :: String -> Char -> (Char, String)
f s x = if x `elem` s then (x, s) else (toUpper x, x : s)

compose :: [(a -> a)] -> a -> a
compose = foldr (.) id

fib :: Integer -> Integer
fib n = fst $ foldr (\_ (pprev,prev) -> (prev,pprev+prev)) (0,1) [1..n]

length':: [a] -> Integer
length' = foldr (const (+1)) 0

reverse':: [a] -> [a]
reverse' = foldr (\x y -> y++[x]) []

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> (f x):acc) [] xs

inits' :: [a] -> [[a]]
inits' xs = snd $ foldr (\_ (xs,acc) -> (init xs,xs:acc)) (xs,[]) [0..length' xs]
