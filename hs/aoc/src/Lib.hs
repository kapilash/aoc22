module Lib
    ( someFunc,
    maxCalorie,
    max3Calories,
    day2Puzzle1,
    day3Puzzle1,
    day3Puzzle2,
    day4Puzzle1,
    day4Puzzle2,
    day5Puzzle1,
    Day6P1(..),
    updateDay6P1,
    updateDay6P2,
    areUnique,
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


data D4P1 = D4P1 Int Int
            deriving (Show, Eq)

readNum :: Monad m => ParsecT String u m Int
readNum = do
    n <- many1 digit
    return $ read n

d4p1 :: Monad m => ParsecT String u m D4P1
d4p1 = do
    n1 <- readNum
    char '-'
    n2 <- readNum
    return $ D4P1 n1 n2

d4p1Pair :: Monad m => ParsecT String Int m ()
d4p1Pair = do
    d1 <- d4p1
    char ','
    d2 <- d4p1
    eol
    modifyState (\x -> x + (changeStateD4P1 d1 d2))
    return ()

isContained :: D4P1 -> D4P1 -> Bool
isContained (D4P1 a b) (D4P1 c d) = ((a >= c) && (b <= d)) || ((a <= c) && (b >= d))

isOverlap :: D4P1 -> D4P1 -> Bool
isOverlap (D4P1 a b) (D4P1 c d) = ((a <= c) && (b >= c)) || ((a <= d) && (b >= c))

changeStateD4P1 :: D4P1 -> D4P1 -> Int
changeStateD4P1 d1 d2 = if (isContained d1 d2) then 1 else 0

changeStateD4P2 :: D4P1 -> D4P1 -> Int
changeStateD4P2 d1 d2 = if (isOverlap d1 d2) then 1 else 0

d4p2Pair :: Monad m => ParsecT String Int m ()
d4p2Pair = do
    d1 <- d4p1
    char ','
    d2 <- d4p1
    eol
    modifyState (\x -> x + (changeStateD4P2 d1 d2))
    return ()

d4p2All :: Monad m => ParsecT String Int m Int
d4p2All = do
  many1 d4p2Pair
  eof
  i <- getState
  return i

day4Puzzle2 :: FilePath -> IO ()
day4Puzzle2 f = do
   input <- readFile f
   case (runParser d4p2All 0 f input) of
    Left err -> print err
    Right v  -> print v


d4p1All :: Monad m => ParsecT String Int m Int
d4p1All = do
  many1 d4p1Pair
  eof
  i <- getState
  return i

day4Puzzle1 :: FilePath -> IO ()
day4Puzzle1 f = do
   input <- readFile f
   case (runParser d4p1All 0 f input) of
    Left err -> print err
    Right v  -> print v

newtype D5P1 = D5P1 [[Char]] 
            deriving (Show, Eq)

type RawStacks = [[Maybe Char]]


maybeChars :: [Maybe Char] -> [Char]
maybeChars [] = []
maybeChars (Nothing : xs) = maybeChars xs
maybeChars ((Just c) : xs) = c : (maybeChars xs)

rawStacksToStacks :: [[Maybe Char]] -> [[Char]]
rawStacksToStacks ([]:_) = []
rawStacksToStacks x = (maybeChars . map head $ x) : (rawStacksToStacks (map tail x))

getHeads :: D5P1 -> [Char]
getHeads (D5P1 s) = map head s

d5crate :: Monad m => ParsecT String D5P1 m (Maybe Char)
d5crate = do
    char '['
    c <- anyChar
    char ']'
    return (Just c)

d5Empty :: Monad m => ParsecT String D5P1 m (Maybe Char)
d5Empty = do
    char ' '
    char ' '
    char ' '
    return Nothing

d5BoxOrEmpty :: Monad m => ParsecT String D5P1 m (Maybe Char)
d5BoxOrEmpty =    d5crate <|> d5Empty

d5ReadCrates :: Monad m => ParsecT String D5P1 m [Maybe Char]
d5ReadCrates = do
    line <- sepBy1 d5BoxOrEmpty (char ' ')
    eol
    return line

d5NumberLine :: Monad m => ParsecT String D5P1 m ()
d5NumberLine = do
    many1 (char ' ' <|> digit)
    eol
    return ()

d5EmptyLine :: Monad m => ParsecT String D5P1 m ()
d5EmptyLine = do
    many (char ' ')
    eol
    return ()

d5Move :: Int -> Int -> Int -> D5P1 -> D5P1
d5Move i from to (D5P1 stacks) = 
    let
       moved = d5MHelper' i (stacks !! from, stacks !! to)
    in
      D5P1 (d5MHelper2 0 moved (from,to) stacks)

d5MHelper :: Int -> ([Char], [Char]) -> ([Char], [Char])
d5MHelper 0 (from, to) = (from, to)
d5MHelper i (from, to) = d5MHelper (i - 1) (tail from, (head from):to)

d5MHelper2 :: Int -> ([Char], [Char]) -> (Int, Int) -> [[Char]] -> [[Char]]
d5MHelper2 _ _ _ [] = []
d5MHelper2 i (from, to) (fromI, toI) (curr:rest) = if i == fromI then
                                                    from:(d5MHelper2 (i + 1) (from, to) (fromI, toI) rest)
                                                  else if i == toI then
                                                    to:(d5MHelper2 (i + 1) (from, to) (fromI, toI) rest)
                                                  else
                                                    curr:(d5MHelper2 (i + 1) (from, to) (fromI, toI) rest)

d5MHelper' :: Int -> ([Char], [Char]) -> ([Char], [Char])
d5MHelper' i (from, to) =  (drop i from, (take i from) ++ to)

d5MoveLine :: Monad m => ParsecT String D5P1 m ()
d5MoveLine = do
    string "move "
    i <- readNum
    string " from "
    from <- readNum
    string " to "
    to <- readNum
    eol
    modifyState (d5Move i (from - 1) (to - 1))
    return ()

d5ReadMoves :: Monad m => ParsecT String D5P1 m ()
d5ReadMoves = do
    many1 d5MoveLine
    eof
    return ()

d5ReadAll :: Monad m => ParsecT String D5P1 m String
d5ReadAll = do
    matrix <- manyTill d5ReadCrates (try d5NumberLine)
    many1 d5EmptyLine
    putState $ D5P1 (rawStacksToStacks matrix)
    d5ReadMoves
    eof
    s <- getState
    return (getHeads s)

day5Puzzle1 :: FilePath -> IO ()
day5Puzzle1 f = do
   input <- readFile f
   case (runParser d5ReadAll (D5P1 []) f input) of
    Left err -> print err
    Right v  -> print v

data Day6P1 = Day6P1 Bool Int String
            deriving (Show, Eq)

areUnique :: [Char] -> Bool
areUnique [] = True
areUnique (x:xs) = if x `elem` xs then False else areUnique xs

updateDay6P1 ::  Day6P1 -> Char -> Day6P1
updateDay6P1 (Day6P1 True i s) c  = Day6P1 True i s
updateDay6P1 (Day6P1 False i "") c = Day6P1 False (i + 1) [c]
updateDay6P1 (Day6P1 False i s) c = let
                                      newstr = if length s < 4 then s ++ [c] else (tail s) ++ [c]
                                      found = areUnique newstr && (length newstr) == 4
                                    in
                                        Day6P1 found (i+1) newstr

updateDay6P2 ::  Day6P1 -> Char -> Day6P1
updateDay6P2 (Day6P1 True i s) c  = Day6P1 True i s
updateDay6P2 (Day6P1 False i "") c = Day6P1 False (i + 1) [c]
updateDay6P2 (Day6P1 False i s) c = let
                                      newstr = if length s < 14 then s ++ [c] else (tail s) ++ [c]
                                      found = areUnique newstr && (length newstr) == 14
                                    in
                                        Day6P1 found (i+1) newstr

