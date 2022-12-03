module Lib
    ( someFunc,
    maxCalorie,
    max3Calories,
    ) where

import qualified Data.List as Lst 
import Text.Parsec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type CalParser s = Parsec s Int

eol :: Stream s m Char => ParsecT s u m ()
eol = do
    newline <|> crlf
    return ()

calorieLine :: Stream s m Char => ParsecT s u m Int
calorieLine = do
   num <- many1 digit
   eol
   return $ read num

calorieChunk :: Stream s m Char => ParsecT s u m Int
calorieChunk = do
  calories <- many1 calorieLine
  eol
  return $ sum calories

elfCalorie1 :: Stream s m Char => ParsecT s Int m ()
elfCalorie1 = do
  next <- calorieChunk
  modifyState (\x -> max x next)
  return ()

changeStateIfNeeded :: Int -> (Int, Int, Int) -> (Int, Int,Int)
changeStateIfNeeded curr (f, s, t) = if curr >= f then (curr, f, s)
                                     else if curr >= s then (f, curr, s)
                                          else if curr >= t then (f, s, curr)
                                               else (f, s, t)

elfCalorie2 :: Stream s m Char => ParsecT s (Int,Int,Int) m ()
elfCalorie2 = do
  next <- calorieChunk
  modifyState (changeStateIfNeeded next)
  return ()

allCalories1 :: Stream s m Char => ParsecT s Int m Int
allCalories1 = do
  many1 (try elfCalorie1)
  i <- getState
  return i

allCalories2 :: Stream s m Char => ParsecT s (Int,Int,Int) m (Int,Int,Int)
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

--calorieLists :: FilePath -> IO [String]
calorieLists f = do
    text <- readFile f
    return $  words  text

