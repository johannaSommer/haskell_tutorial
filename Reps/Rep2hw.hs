module Rep2hw where

class Semigroup a where
  (<>) :: a -> a -> a
class Semigroup m => Monoid m where
  mempty :: m

data Pair a = Pair a a

instanceOf Semigroup a => Semigroup (Pair a) where
  (Pair a1 a2) <> (Pair b1 b2) = Pair (a1 <> b1) (a2 <> b2)

instanceOf Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead x:xs = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs

safeHead :: [a] -> Maybe a
safeHead xs = foldr (\h _ -> Just h) Nothing xs

safeLast :: [a] -> Maybe a
safeLast xs = foldr aux Nothing xs
  where
    aux l Nothing = Just l
    aux r l = l


select :: Eq a => a -> [(a,b)] -> [b]
select a xs = map (\(x, y) -> y) $ filter (\(x, y) -> x == a) xs

data Node = File String | Dir String [Node]
type FileSys = [Node]

removeFiles :: String -> FileSys -> FileSys
removeFiles _ [] = []
removeFiles name (File name' : fs) =
  (if name' == name then [] else [File name']) ++ removeFiles name fs
removeFiles name (Dir name' fs' : fs) =
  Dir name' (removeFiles name fs') : removeFiles name fs

type Name = String

{-2.2.3a-}
data Atom' = T' | V' Name
  deriving (Eq, Show)
data Literal = Pos Atom' | Neg Atom'
  deriving (Eq, Show)
data Formula = L Literal | Formula :&&: Formula | Formula :||: Formula
  deriving (Eq, Show)

{-2.2.3b-}
top :: Literal
top = Pos T'

bottom :: Literal
bottom = Neg T'

{-2.2.3c-}
type Clause = [Literal]
type CNF = [Clause]

clauseToForm :: Clause -> Formula
clauseToForm [] = L bottom
clauseToForm ls = foldr ((:||:) . L) (L $ last ls) (init ls)

conjToForm :: CNF -> Formula
conjToForm [] = L top
conjToForm ds = foldr ((:&&:) . clauseToForm) (clauseToForm $ last ds) (init ds)

{-2.2.3d-}
type Valuation = [(Name,Bool)]

substLiteral :: Valuation -> Literal -> Literal
substLiteral v l@(Pos (V' n)) = case lookup n v of
  Just b  -> if b then top else bottom
  Nothing -> l
substLiteral v l@(Neg (V' n)) = case lookup n v of
  Just b  -> if b then bottom else top
  Nothing -> l
substLiteral v l = l

substClause :: Valuation -> Clause -> Clause
substClause = map . substLiteral

substConj :: Valuation -> CNF -> CNF
substConj = map . substClause

{-2.2.3e-}
simpClause :: Clause -> Clause
simpClause = foldr collect []
  where
    collect _ [l] | l == top    = [top]
    collect l _   | l == top    = [top]
    collect l acc | l == bottom = acc
    collect a acc               = a:acc

simpConj :: CNF -> CNF
simpConj = foldr collect [] . map simpClause
  where
    collect _   [[]]           = [[]]
    collect []  _              = [[]]
    collect [l] acc | l == top = acc
    collect a   acc            = a:acc
