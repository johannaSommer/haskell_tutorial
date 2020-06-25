import Data.List (group, minimum, minimumBy, nub, replicate, union, words)

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
takeWhile' f (x:xs) 
  | f x = x:(takeWhile' f xs)
  | otherwise = [] 

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr ((:) . f) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr ((\y ys -> if (f y) then y:ys else ys)) [] xs

compose :: [(a -> a)] -> a -> a
compose = foldr (.) id 

remdups :: Eq a => [a] -> [a]
remdups xs = reverse $ aux $ reverse xs 

aux :: Eq a => [a] -> [a]
aux [] = []
aux (x:xs) 
  | elem x xs = [] ++ (aux xs)
  | otherwise = x:(aux xs)

iter :: Int -> (a -> a) -> a -> a
iter n f x
  | n <= 0    = x
  | otherwise = iter (n - 1) f (f x)

pow' :: Int -> Int -> Int
pow' n k = iter k (\a -> a * a) n

drop' :: Int -> [a] -> [a] 
drop' n xs = iter n (\(x:xs) -> xs) xs

halfEven :: [Int] -> [Int] -> [Int] 

halfEven xs ys = [x + y | (x, y) <- (zip xs ys), even (x+y)]

halfEven' :: [Int] -> [Int] -> [Int] 
halfEven' xs [] = []
halfEven' [] ys = []
halfEven' (x:xs) (y:ys)
  | even (x+y) = (x+y):(halfEven xs ys)
  | otherwise = halfEven xs ys


halfEven'' :: [Int] -> [Int] -> [Int] 
halfEven'' xs ys = filter even $ map (\(x, y) -> x+y) (zip xs ys)

data Term = App Term Term | Abs String Term | Var String

instance Show Term where
  show (Var x) = x
  show (Abs x t) = "(\\" ++ x ++ " -> " ++ show t ++ ")"
  show (App t1 t2@(App _ _)) = show t1 ++ " (" ++ show t2 ++ ")"
  show (App t1 t2) = show t1 ++ " " ++ show t2

freeVars :: Term -> [String]
freeVars (Var s) = s
freeVars (Abs s t) = s:(freeVars t)
freeVars (App t1 t2) = (freeVars t1):(freeVars t2)

data Either a b = Left a | Right b

g :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
g f _ (Left  x) = Left  (f x)
g _ f (Right x) = Right (f x)

h :: (a -> Either a b) -> a -> b
h f a 
  | f a == Right b = b
  | otherwise = h f (f a)