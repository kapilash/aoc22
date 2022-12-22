module Lib
    ( someFunc,
    maxCalorie,
    max3Calories,
    day2Puzzle1,
    day3Puzzle1,
    day3Puzzle2,
    day2Puzzle2
    ) where

import qualified Data.List as Lst 
import Text.Parsec
import Data.Char 
import qualified Data.Set as Set
import qualified Data.List as Lst

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type CalParser  = Parsec String Int

eol :: Monad m => ParsecT String u m ()
eol = do
    newline <|> crlf
    return ()

calorieLine :: Monad m => ParsecT String u m Int
calorieLine = do
   num <- many1 digit
   eol
   return $ read num

calorieChunk :: Monad m => ParsecT String u m Int
calorieChunk = do
  calories <- many1 calorieLine
  eol
  return $ sum calories

elfCalorie1 :: Monad m => ParsecT String Int m ()
elfCalorie1 = do
  next <- calorieChunk
  modifyState (\x -> max x next)
  return ()

changeStateIfNeeded :: Int -> (Int, Int, Int) -> (Int, Int,Int)
changeStateIfNeeded curr (f, s, t) = if curr >= f then (curr, f, s)
                                     else if curr >= s then (f, curr, s)
                                          else if curr >= t then (f, s, curr)
                                               else (f, s, t)

elfCalorie2 :: Monad m => ParsecT String (Int,Int,Int) m ()
elfCalorie2 = do
  next <- calorieChunk
  modifyState (changeStateIfNeeded next)
  return ()

allCalories1 :: Monad m => ParsecT String Int m Int
allCalories1 = do
  many1 (try elfCalorie1)
  i <- getState
  return i

allCalories2 :: Monad m => ParsecT String (Int,Int,Int) m (Int,Int,Int)
allCalories2 = do
  many1 (try elfCalorie2)
  i <- getState
  return i

maxCalorie :: FilePath -> IO ()
maxCalorie f = do
   input <- readFile f
   case (runParser allCalories1 0 f input) of
    Left err -> print err
    Right v  -> print v

max3Calories :: FilePath -> IO ()
max3Calories f = do
   input <- readFile f
   case (runParser allCalories2 (0,0,0) f input) of
    Left err -> print err
    Right (v1,v2,v3)  -> print $ v1 + v2 + v3

data Day2 = Rock 
            | Paper 
            | Scissors
            deriving (Eq, Show)


data Day2P2 = Win
              | Lose
              | Tie
              deriving (Eq, Show)

shapeScore :: Day2 -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore _ = 3

outcome :: Day2 -> Day2 -> Int
outcome Rock Rock = 3
outcome Rock Paper = 6
outcome Rock Scissors     = 0
outcome Paper Rock = 0
outcome Paper Paper = 3
outcome Paper Scissors    = 6
outcome Scissors Rock = 6
outcome Scissors Paper = 0
outcome Scissors Scissors  = 3


p2outcome :: Day2 -> Day2P2 -> Int
p2outcome Rock Win = 6 + (shapeScore Paper)
p2outcome Rock Tie = 3 + (shapeScore Rock)
p2outcome Rock Lose = 0 + (shapeScore Scissors)
p2outcome Paper Win = 6 + (shapeScore Scissors)
p2outcome Paper Tie = 3 + (shapeScore Paper)
p2outcome Paper Lose = 0 + (shapeScore Rock)
p2outcome Scissors Win = 6 + (shapeScore Rock)
p2outcome Scissors Tie = 3 + (shapeScore Scissors)
p2outcome Scissors Lose = 0 + (shapeScore Paper)


genTurn :: Monad m => Char -> Day2 -> ParsecT String u m Day2
genTurn c d = do
   char c
   return d

theirTurn :: Monad m => ParsecT String u m Day2
theirTurn = do
   theirs <- (genTurn 'A' Rock) <|> (genTurn 'B' Paper) <|> (genTurn 'C' Scissors)
   many1 space
   return theirs 

genTurn2 :: Monad m => Char -> Day2P2 -> ParsecT String u m Day2P2
genTurn2 c d = do
    char c
    return d

myTurn2 :: Monad m => ParsecT String u m Day2P2
myTurn2 = do
    mine <- (genTurn2 'X' Lose) <|> (genTurn2 'Y' Tie) <|> (genTurn2 'Z' Win)
    eol
    return mine


myTurn :: Monad m => ParsecT String u m Day2
myTurn = do
   mine <- (genTurn 'X' Rock) <|> (genTurn 'Y' Paper) <|> (genTurn 'Z' Scissors)
   eol
   return mine


oneGame :: Monad m => ParsecT String Int m ()
oneGame = do
   theirChoice <- theirTurn
   myChoice <- myTurn
   modifyState (\x -> x + (outcome theirChoice myChoice) + (shapeScore myChoice))
   return ()

allGames :: Monad m => ParsecT String Int m Int
allGames = do
  many1 oneGame
  eof
  i <- getState
  return i

day2Puzzle1 :: FilePath -> IO ()
day2Puzzle1 f = do
   input <- readFile f
   case (runParser allGames 0 f input) of
    Left err -> print err
    Right v  -> print v

oneGame2 :: Monad m => ParsecT String Int m ()
oneGame2 = do
   theirChoice <- theirTurn
   myChoice <- myTurn2
   modifyState (\x -> x + (p2outcome theirChoice myChoice))
   return ()

allGames2 :: Monad m => ParsecT String Int m Int
allGames2 = do
  many1 oneGame2
  eof
  i <- getState
  return i

day2Puzzle2 :: FilePath -> IO ()
day2Puzzle2 f = do
   input <- readFile f
   case (runParser allGames2 0 f input) of
    Left err -> print err
    Right v  -> print v

priority :: Char -> Int
priority c = if c >= 'a' && c <= 'z' then 1 + (ord c) - (ord 'a')
                 else if c >= 'A' && c <= 'Z' then 27 + (ord c) - (ord 'A')
                      else 0

rucksackPriority :: String -> Int
rucksackPriority s = let 
                    (left, right) = Lst.splitAt (div (length s)  2) s
                   in
                   priority . Set.elemAt 0 $ Set.intersection (Set.fromList left) (Set.fromList right) 

day3Puzzle1 :: FilePath -> IO ()
day3Puzzle1 f = do
   input <- readFile f
   print $ sum $ map rucksackPriority $ lines input

commonChars :: [String] -> Set.Set Char
commonChars  = foldl (\x y -> Set.intersection x (Set.fromList y)) initialSet
            where
                initialSet = Set.union (Set.fromList ['a' .. 'z']) (Set.fromList ['A' .. 'Z'])

groupScore :: [String] -> Int
groupScore [] = 0
groupScore s = let 
                (curr, next) = Lst.splitAt 3 s
                currValue = priority . Set.elemAt 0 $ commonChars curr
              in
                case next of
                   [] -> currValue
                   _ -> currValue + (groupScore next)

day3Puzzle2 :: FilePath -> IO ()
day3Puzzle2 f = do
    input <- readFile f
    print $ groupScore (lines input)
