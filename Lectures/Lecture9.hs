module Lecture9 where

data NonEmptyList a = Single a | Cons a (NonEmptyList a)
  deriving (Show, Eq)

fromList :: [ a ] -> Maybe ( NonEmptyList a )
fromList [] = Nothing
fromList (x : xs) = Just (go x xs)
  where go x [] = Single x
        go x (y : ys) = Cons x (go y ys)

toList :: NonEmptyList a -> [ a ]
toList Single x = [x]
toList Cons x y = [x] ++ (toList y)


nHead :: NonEmptyList a -> a
nHead Single x = x
nHead Cons x xs = x

nTail :: NonEmptyList a -> Maybe (NonEmptyList a)
nTail Single _ = Nothing
nTail Cons _ xs = Just xs

nAppend :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
nAppend (Single a) xs = Cons a xs
nAppend (Cons x y) xs= Cons x (nAppend y xs)

nTake :: Integer -> NonEmptyList a -> Maybe (NonEmptyList a)
nTake n _ | n <= 0 = Nothing
nTake n (Single a)
  | n == 1 = Just $ Single a
  | otherwise = Nothing
nTake n (Cons a tl)
  | n == 1 = Just (Single a)
  | otherwise = case nTake (n-1) tl of
                Nothing -> Nothing
                Just res -> Just $ Cons a res
