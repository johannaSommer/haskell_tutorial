threeAscending :: Integer -> Integer -> Integer -> Bool
threeAscending x y z = x < y && y < z

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual u v w x = u == v && v == w && w == x

fac :: Integer -> Integer
fac 1 = 1
fac n | n > 0 = n * fac (n - 1)

sumEleven :: Integer -> Integer
sumEleven n = sumEleven' n 10

sumEleven' :: Integer -> Integer -> Integer
sumEleven' n 0 = n
sumEleven' n i = n + i + sumEleven' n (i - 1)

argMax :: ( Integer -> Integer ) -> Integer -> Integer
argMax g n = argMax' g n 0

argMax' :: (Integer -> Integer) -> Integer -> Integer -> Integer
argMax' g 0 m = m
argMax' g n m | n > 0 = argMax' g (n-1) temp
  where temp = if g n > g m then n else m

g :: Integer -> Integer
g n = if n < 10 then n * n else n

myPair :: Integer -> Integer -> Integer
myPair x y = 2 ^ x * (2 * y + 1)

myFst :: Integer -> Integer
myFst x 
  | x `mod` 2 == 0 = 1 + myFst (x `div` 2) 
  | otherwise = 0

mySnd :: Integer -> Integer
mySnd x = (x `div` 2^(myFst x) - 1) `div` 2

equivMod :: Integer -> Integer -> Integer -> Bool
equivMod n a b = a == mod b n

quadRes :: Integer -> Integer -> Bool
quadRes n a = quadResAux n a (n `div` 2)
  where
    quadResAux _ _ 0 = equivMod n a 0
    quadResAux n a x = equivMod n a (x^2) || quadResAux n a (x - 1)


prime :: Integer -> Bool
prime n = prime' n 2

prime' :: Integer -> Integer -> Bool
prime' n i 
  | i >= n = True
  | mod n i == 0 = False
  | otherwise = prime' n (i+1)
  