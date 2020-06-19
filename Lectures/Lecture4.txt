import Data.List (group, minimum, minimumBy, nub, replicate, union, words)

andList :: [String] -> String
andList (x:y:z:[]) = x ++ ", " ++ y ++ ", and " ++ z
andList (x:y:[]) = x ++ " and " ++ y
andList (x:[]) = x
andList [] = ""
andList (x:xs) = x ++ ", " ++ (andList xs)

lSub :: Num a => [a] -> [a] 
lSub [] = []
lSub (x:[]) = [x]
lSub (x:y:xs) = [(x - y)] ++ lSub (y:xs) 

noDubSnoc :: Eq a => [a] -> a -> [a] 
noDubSnoc [] y = [y]
noDubSnoc (x:[]) y = if x == y then [x] else [x, y]
noDubSnoc (x:xs) y = if x==y then (x:xs) else [x] ++ (noDubSnoc xs y) 

isMultiSet :: Eq a => [(a, Int)] -> Bool
isMultiSet xs = ((length $ nub $ firsts) == (length firsts)) && (and [x > 0 | (_, x) <- xs])
  where 
    firsts = [x | (x, _) <- xs]

toList :: [(a, Int)] -> [a]
toList [] = []
toList ((a, b):xs) = (replicate b a) ++ toList xs

toSet :: Eq a => [(a, Int)] -> [a]
toSet mx = nub $ [ e | (e, _) <- mx ] 

toMultiSet :: Eq a => [a] -> [(a, Int)]
toMultiSet xs = nub [(x, (count xs x)) | x <- xs]

count :: Eq a => [a] -> a -> Int
count [] a = 0
count (x:xs) a
  |x == a = 1 + count xs a 
  |otherwise =  count xs a 

multiplicity :: Eq a => a -> [(a, Int)] -> Int
multiplicity _ [] = 0
multiplicity e ((e', m):xs) = if e == e' then m else multiplicity e xs

dotProduct :: Eq a => [(a, Int)] -> [(a, Int)] -> Int
dotProduct mx my = sum [ multiplicity e mx * multiplicity e my | e <- toSet mx `union` toSet my ]

euclidean :: Eq a => [(a, Int)] -> Float
euclidean mx = sqrt $ fromIntegral (dotProduct mx mx)

cosine :: Eq a => [(a, Int)] -> [(a, Int)] -> Float
cosine mx my = fromIntegral (dotProduct mx my) / (euclidean mx * euclidean my)
