threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = (x /= y) && (y /= z) && (x /= z)

pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x n = x * (pow x (n-1))

ascending :: Ord a => [a] -> Bool
ascending (x:y:[]) = x < y
ascending (x:y:xs) = (x < y) && (ascending  (y:xs))

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ (zip' xs ys)

fac :: Int -> Int
fac n = aux 1 n
  where
  aux m 0 = m
  aux m n = (m*n) * aux m (n-1)

concat' :: [[a]] -> [a]
concat' xs = [y | x<-xs, y<-x]

primes :: Int -> [Int]
primes n = [x | x <- [1 .. n], isPrime x]

isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False

matches :: Integer -> [Integer] -> [Integer]
matches n xs = [x | x <- xs, x == n]

elem' :: Integer -> [Integer] -> Bool
elem' y xs = not $ null $ matches y xs 

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = 
  | f x = x:(takeWhile' xs)
  | otherwise = [] 

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr ((:) . f) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr ((\y ys -> if (f y) then y:ys else ys) [] xs

compose :: [(a -> a)] -> a -> a
compose = foldr (.) id 

fib :: Integer -> Integer