import Data.List (group, minimum, minimumBy, nub, replicate, union, words)

multiply :: Int -> Int -> Int
multiply 0 b = 0
multiply a b = b + multiply (a-1) b

squareroot :: Int -> Int
squareroot n = aux n n
  where
  aux a n 
    | a*a <= n = a
    | otherwise = aux (a-1) n

argMax :: (Integer -> Integer) -> Integer -> Integer
argMax _ 0 = 0
argMax g n | n > 0 =
  let rec_max = argMax g (n - 1)
  in if g rec_max > g n then rec_max else n

concat' :: [[a]] -> [a]
concat' xss = aux [] xss
  where
  aux acc [] = acc
  aux acc (xs:xss) = aux (acc ++ xs) xss 

sum' :: [Integer] -> Integer
sum' xs = aux 0 xs
  where  
    aux acc [] = acc
    aux acc (x:xs) = aux (acc + x) xs

duplicate :: String -> Integer -> String
duplicate s n 
  | n <= 0 = ""
  | n == 1 = s
  | otherwise = concat [s | _ <- [1 .. n]]

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
  | p x       = dropWhile' p xs
  | otherwise = x:xs

iterWhile :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterWhile test f x =
  let x' = f x in
  if test x x' then iterWhile test f x'
               else x

length' :: [a] -> Integer
length' = foldr (const (+1)) 0

reverse' :: [a] -> [a]
reverse' = foldr (\x -> (++[x])) []

inits' :: [a] -> [[a]]
inits' xs = snd $ foldr (\_ (xs,acc) -> (init xs,xs:acc)) (xs,[]) [0..length' xs]

zip' :: [a] -> [b] -> [(a,b)]
zip' xs [] = []
zip' [] ys = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)
unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([],[])
unzip' ((x,y):zs) =
  let (xs,ys) = unzip' zs
  in (x:xs,y:ys)

zip'' :: [a] -> [b] -> [(a,b)]
zip'' xs ys
  | length xs == length ys = [(xs !! i, ys !! i) | i <- [0..length xs - 1]]
  | otherwise              = []
unzip'' :: [(a,b)] -> ([a], [b])
unzip'' xs = ([x | (x,_) <- xs],[x | (_,x) <- xs])

zip''' :: [a] -> [b] -> [(a,b)]
zip''' xs ys
  | length xs == length ys = map (\i -> (xs !! i, ys !! i)) [0..length xs - 1]
  | otherwise              = []
unzip''' :: [(a,b)] -> ([a], [b])
unzip''' = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[])

f :: [Int] -> [Int]
f [] = []
f (x:xs) 
  | x < 0 = (abs x):(f xs)
  | otherwise = f xs

f' :: [Int] -> [Int]
f' xs = [abs x | x <- xs, x < 0]

f'' :: [Int] -> [Int]
f'' xs = map abs $ filter (<0) xs
