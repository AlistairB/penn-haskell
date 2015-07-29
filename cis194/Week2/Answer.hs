{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = sum $ zipWith (\x y -> if x == y then 1 else 0) xs ys

-- Nick's solution - exactMatches xs ys = length . (filter id) $ zipWith (==) xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors [] = []
countColors xs = map (\x -> length $ filter (==x) xs) colors

-- matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] == 3

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith getMatchCount guessCount actualCount
  where
    guessCount = countColors xs
    actualCount = countColors ys
    getMatchCount x y = if x > y then y else x

-- Exercise 3 -----------------------------------------

--getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] == Move [Red, Orange, Orange, Blue] 1 2

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove guess actual = Move guess exactMatchesCount nonExactMatchesCount
  where
    exactMatchesCount = exactMatches guess actual
    nonExactMatchesCount = (matches guess actual) - exactMatchesCount -- a non exact match includes the exact matches so we remove them


-- Exercise 4 -----------------------------------------

-- isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple] == True
-- isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple] == False

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exactCount nonExactCount) actual = exactCount == newExactCount && nonExactCount == newNonExactCount
  where
    newExactCount = exactMatches guess actual
    newNonExactCount = (matches guess actual) - newExactCount

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes length = undefined

-- Exercise 7 -----------------------------------------

-- solve :: Code -> [Move]
-- solve = undefined

-- Bonus ----------------------------------------------

-- fiveGuess :: Code -> [Move]
-- fiveGuess = undefined
