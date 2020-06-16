allSums :: [Integer] -> [Integer] -> [Integer]
allSums xs ys = [x + y | x <- xs, y <- ys]

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, even x]

nLists :: [Integer] -> [[Integer]]
nLists xs = [[1 .. x] | x <- xs]

allEvenSumLists :: [ Integer ] -> [ Integer ] -> [[ Integer ]]
allEvenSumLists xs ys = nLists (evens (allSums xs ys))

toSet :: [Integer] -> [Integer]
toSet [] = []
toSet xs
  | elem (head xs) (tail xs) = toSet (tail xs)
  | otherwise = [head xs] ++ toSet (tail xs)

isSet :: [Integer] -> Bool
isSet xs = length xs == (length $ toSet xs)

union :: [Integer] -> [Integer] -> [Integer]
union xs ys = toSet $ xs ++ ys

intersection :: [Integer] -> [Integer] -> [Integer]
intersection xs ys = toSet [x | x <- xs ++ ys, elem x xs, elem x ys]

diff :: [Integer] -> [Integer] -> [Integer]
diff xs ys = [x | x <- xs, not $ elem x ys]

eqFrac :: (Integer,Integer) -> (Integer,Integer) -> Bool
eqFrac (a,b) (c,d) = a*d == c * b


 