
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [a] = []
localMaxima (x:y:[]) = []
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (z:xs)
  | otherwise      = localMaxima (y:z:xs)

--processMaxima n a f
