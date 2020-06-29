module Lecture10 where
import Data.List
--import Test.QuickCheck

{-H10.1-}
data Player = V | H -- vertical or horizontal player
  deriving (Eq,Show)
data Field = P Player | E -- a field is occupied by a player or empty
  deriving (Eq,Show)
type Row = [Field]
type Column = [Field]
type Board = [Row] -- we assume that boards are squares and encode a board row by row
data Game = Game Board Player -- stores the current board and player
  deriving (Eq,Show)

-- get a given row of a board
row :: Board -> Int -> Row
row = (!!)

-- get a given column of a board
column :: Board -> Int -> Column
column = row . transpose

-- width of a board
width :: Board -> Int
width [] = 0
width (x:xs) = length x

-- height of a board
height :: Board -> Int
height = length

{-H10.1.1-}
prettyShowBoard :: Board -> String
prettyShowBoard [] = ""
prettyShowBoard (x:xs) = (printRow x) ++ (prettyShowBoard xs)

printRow :: Row -> String
printRow [] = "\n"
printRow (x:xs) = (printField x) ++ (printRow xs)

printField :: Field -> String
printField (P H) = "H"
printField (P V) = "V"
printField E = "+"

{-H10.1.2-}
-- position on a board (row, column)
-- (0,0) corresponds to the top left corner
type Pos = (Int, Int)

isValidMove :: Game -> Pos -> Bool
isValidMove (Game b H) (r,c) =  ((height b) > (r+1)) && ((width b) > c) && (b !! r !! c == E) && (b !! (r+1) !! c == E)
isValidMove (Game b V) (r,c) =  ((height b) > r) && ((width b) > (c+1)) && (b !! r !! c == E) && (b !! r !! (c+1) == E)

{-H10.1.3-}
canMove :: Game -> Bool
canMove g@(Game b _) = or [isValidMove g (r, c) | r <- [0 .. (height b)], c <- [0 .. (width b)]]


{-H10.1.4-}
updateBoard :: Board -> Pos -> Field -> Board
updateBoard b (r, c)  f = aux b r
    where
    aux (x:xs) 0 = (updateRow x c f) : xs
    aux (x:xs) r = x : aux xs (r-1)

updateRow :: Row -> Int -> Field -> Row
updateRow (x:xs) 0 f = f : xs
updateRow (x:xs) c f = x : (updateRow xs (c-1) f)

{-H10.1.5-}
playMove :: Game -> Pos -> Game
playMove (Game b p) pos = let newplayer = if p == H then V else H in (Game (updateBoard b pos (P p)) newplayer)

{-H10.1.6-}
-- the first paramter of a strategy is an infite list of
-- random values between (0,1) (in case you wanna go wild with
-- probabilistic methods)
type Strategy = [Double] -> Game -> Pos

{-WETT-}
christmasAI :: Strategy -- receives a game and plays a move for the next player
christmasAI = undefined
{-TTEW-}

{-H10.1.7-}
play :: [[Double]] -> Int -> Strategy -> Strategy -> ([Board],Player)
play = undefined

