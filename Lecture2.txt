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

removeInvalidGuesses :: [( String , Int )] -> [( String , Int )]
removeInvalidGuesses xs = [(n, g) | (n, g) <- xs, n/="", 0<=g, g<=100]

average :: [( String , Int )] -> Int
average [] = 0
average xs = div (sum' xs) (length xs)

sum' :: [( String , Int )] -> Int
sum' [] = 0
sum' xs = (snd $ head xs) + (sum' $ tail xs)

winners :: [(String,Int)] -> [String]
winners gs = [n | (n,g) <- gs', abs(g - avg) == minDiff]
    where
        gs' = removeInvalidGuesses gs
        avg = average gs'
        minDiff = minimum [abs(g-avg) | (_,g) <- gs']
 

power :: [ Integer ] -> [[ Integer ]]
power xs = [[]] ++ [[x] | x <- xs] ++ [xs]

subsetEq :: [ Integer ] -> [ Integer ] -> Bool
subsetEq xs ys = (length $ toSet $ xs ++ ys) == length ys

comparable :: [ Integer ] -> [ Integer ] -> Bool
comparable xs ys = (subsetEq xs ys) || (subsetEq ys xs)

isAntichain :: [[ Integer ]] -> Bool
isAntichain xs = length [(x,y) | x <- xs, y <- xs, comparable x y] == length xs

antichains :: Integer -> [[[ Integer ]]]
antichains n = let chain = power n in
  [x | x <- chain, isAntichain x]
  

