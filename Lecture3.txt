dimensions :: [[a]] -> (Int,Int)
dimensions xs
  | null xs = (0,0)
  | and [length (head xs) == length x | x <- xs] = (length xs, length (head xs))
  | otherwise = (-1,-1)

isSquare :: [[a]] -> Bool
isSquare xs = (fst $ dimensions xs) == (snd $ dimensions xs) 

canAdd :: [[a]] -> [[a]] -> Bool
canAdd xs ys = ((fst $ dimensions xs) == (fst $ dimensions ys)) &&
  ((snd $ dimensions xs) == (snd $ dimensions ys))

canMult :: [[a]] -> [[a]] -> Bool
canMult xs ys = ((fst $ dimensions xs) == (fst $ dimensions ys)) 

matrixAdd :: [[Integer]] -> [[Integer]] -> [[Integer]]
matrixAdd xss yss = [[x + y | (x, y) <- zip xs ys] | (xs, ys) <- zip xss yss]

matrixMult :: [[Integer]] -> [[Integer]] -> [[Integer]]
matrixMult a b = 
  if canMult a b
  then let bT = transpose b in
    [ [sum [x*y | (x,y) <- zip aRow bCol] | bCol <- bT ] | aRow <- a] 

mergeSort :: [Integer] -> [Integer]
mergeSort xs =
