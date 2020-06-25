collatz :: Integer -> [Integer] 
collatz n = [n] ++ collatz' n

collatz' 1 = []
collatz' n 
  | even n = (div n 2) : (collatz' $ div n 2)
  | otherwise = (3 * n + 1) : (collatz' ( 3 * n + 1))

unfold :: (a -> Maybe a) -> a -> [a]
unfold f a = a : (aux $ f a)
  where 
    aux Nothing = []
    aux (Just x) = unfold f x


data Tree a = Leaf | Node (Tree a) a (Tree a)

sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node l x r) = x + sumTree l + sumTree r

cut :: Tree a -> Integer -> Tree a
cut (Node l x r) n = aux 0 (Node l x r) n
  where
  aux counter Leaf n = Leaf
  aux counter (Node l x r) n
    | counter < n = Node (aux (counter+1) l n) x (aux (counter+1) l n)
    | otherwise = Leaf 

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f acc Leaf = acc
foldTree f acc (Node l x r) = foldTree f (f (foldTree f acc r) x) l

inorder :: Tree a -> [a]
inorder = foldTree (:) []

findAll :: (a -> Bool) -> Tree a -> [a]
findAll f t = filter f (inorder t)

contains :: Conj -> Atom -> Bool
contains (A a)  c = a == c
contains (c1 :&: c2) a = contains c1 a || contains c2 a