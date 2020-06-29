module Lecture10 where

module Exercise_10 where
import Data.List
import Test.QuickCheck

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
prettyShowField :: Field -> String
prettyShowField (P p) = show p
prettyShowField E = "+"

prettyShowBoard :: Board -> String
prettyShowBoard = unlines . map (concat . map prettyShowField)

{-H10.1.2-}
-- position on a board (row, column)
-- (0,0) corresponds to the top left corner
type Pos = (Int, Int)

isValidPos :: Board -> Pos -> Bool
isValidPos b (r,c) = 0 <= r && r < height b && 0 <= c && c < width b

fieldAt :: Board -> Pos -> Field
fieldAt b (r,c) = row b r !! c

isValidMove :: Game -> Pos -> Bool
isValidMove (Game b H) p@(r,c) = let np = (r,c+1) in
  isValidPos b p && isValidPos b np && [fieldAt b p] `union` [fieldAt b np] == [E]
isValidMove (Game b V) (r,c) = isValidMove (Game (transpose b) H) (c,r)

{-H10.1.3-}
canMove :: Game -> Bool
canMove = undefined

{-H10.1.4-}
updateBoard :: Board -> Pos -> Field -> Board
updateBoard = undefined

{-H10.1.5-}
playMove :: Game -> Pos -> Game
playMove  = undefined

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

-- generates infinite list of values between (0,1)
genRandomZeroOne :: Gen [Double]
genRandomZeroOne = mapM (const $ choose (0::Double,1)) [1..]

-- plays a game and prints it to the console
playAndPrint :: Int -> Strategy -> Strategy -> IO ()
playAndPrint dim sh sv = do
  rss <- generate $ mapM (const $ genRandomZeroOne) [1..]
  let (bs, w) = play rss dim sh sv
  putStr $ (unlines $ map prettyShowBoard bs) ++ "\nWinner: " ++ show w ++ "\n"

