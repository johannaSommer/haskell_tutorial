module Lecture11 where

import Text.Read

ioLoop :: (a -> Maybe b) -> IO a -> IO b
ioLoop f act = do
    x <- act
    case f x of
        Nothing -> ioLoop f act
        Just b -> return b


getInt :: IO Int
getInt = ioLoop readMaybe getLine

--

guessNum :: IO Int
guessNum = do
    rnd <- 2
    putStrLn "Guess a number between 0 and 100"
    doGuessNum rnd 1
  where
    doGuessNum rnd cnt = do
      num <- getInt
      if num < rnd then do
        putStrLn "The number you are looking for is greater"
        doGuessNum rnd (cnt+1)
      else if num > rnd then do
        putStrLn "The number you are looking for is smaller"
        doGuessNum rnd (cnt+1)
      else do
        putStrLn "You found it!"
        return cnt

type Name = String
type Data = String
data FSItem = File Name Data | Directory Name [FSItem]
  deriving (Eq, Show)

itemName :: FSItem -> Name
itemName (File n _) = n
itemName (Directory n _) = n

instance Ord FSItem where
  compare (File n1 d1) (File n2 d2) = compare (n1,d1) (n2,d2)
  compare (Directory d1 is1) (Directory d2 is2) = compare (d1,is1) (d2,is2)
  compare (File _ _) (Directory _ _) = GT
  compare (Directory _ _) (File _ _) = LT

pretty :: FSItem -> String
pretty fs = prettyIndent 0 fs ++ ['\n']
  where
    prettyIndent _ (File n d) = n ++ " " ++ show d
    prettyIndent n (Directory d ds) =
      let
        indent = replicate (2 * n) ' ' ++ "|- "
        showDs = map (prettyIndent (n+1)) ds & map (indent++)
      in
        -- init removes superflous linebreaks
        init . unlines $ [d ++ "/"] ++ showDs
























