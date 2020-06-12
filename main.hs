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