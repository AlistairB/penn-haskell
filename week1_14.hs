mine = 4557025680098999
valid = 4012888888881881
invalid = 4012888888881882

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 9 = [n]
  | otherwise  = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 9 = [n]
  | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther x = reverse (doubleFromLeft (reverse x))
  where
    doubleFromLeft [] = []
    doubleFromLeft (x:[]) = [x]
    doubleFromLeft (x:y:xs) = [x,y*2] ++ (doubleFromLeft xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs)
  | x < 10 = x + (sumDigits xs)
  | otherwise = x - 9 + (sumDigits xs)

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lg Leaf = Node Leaf lg Leaf
insert lg@(LogMessage _ time _) tree =
  let (Node rtree ilog@(LogMessage _ ttime _) ltree) = tree
  in if time > ttime then
    Node rtree ilog (insert lg ltree)
    else Node (insert lg rtree) ilog ltree
