module Golf where

-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []


-- nick how to combine?

skips :: [a] -> [[a]]
skips x = reverse (processEvery x startingLength)
  where
    startingLength = length x

processEvery a n
  | n <= 0 = []
  | otherwise = every n a : (processEvery a (n-1))

every n xs = case drop (n-1) xs of
  (y:ys) -> y : every n ys
  [] -> []
