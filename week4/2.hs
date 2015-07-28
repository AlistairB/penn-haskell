
-- foldTree "ABCDEFGHIJ" ==
--   Node 3
--     (Node 2
--       (Node 0 Leaf ’F’ Leaf)
--       ’I’
--       (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
--     ’J’
--     (Node 2
--       (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--       ’H’
--       (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:[]) = Node 0 Leaf x Leaf
foldTree all@(x:xs) = Node (getLevel all) (foldTree (topHalfList xs)) x (foldTree (bottomHalfList xs))

getLevel :: [a] -> Integer
getLevel [] = -1
getLevel xs = 1 + (getLevel $ topHalfList xs)

topHalfList :: [a] -> [a]
topHalfList [] = []
topHalfList (x:[]) = []
topHalfList xs = take lengthToTake xs
  where
    lengthToTake = length xs `div` 2

bottomHalfList :: [a] -> [a]
bottomHalfList [] = []
bottomHalfList (x:[]) = [x]
bottomHalfList xs = drop lengthToDrop xs
  where
    lengthToDrop = length xs `div` 2
