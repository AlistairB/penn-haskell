fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

awe1 :: [Integer] -> Integer
awe1 = foldl1 (*) . map (\x -> x - 2) . filter even

--fun1' = product . map (subtract 2) . filter even

-- awe2 :: Integer -> Integer
-- awe2 = sum . takeWhile (\x -> x /= 1) $ iterate (\x -> x `div` 2) . work

-- awe2 n | even n = n + fun2 (n `div` 2)
--        | otherwise = fun2 (3 * n + 1)
--awe1 = foldl1 (*) . map (\x -> x - 2) . filter even

awe2 :: Integer -> Integer
awe2 = sum . filter even . takeWhile (/= 1) . iterate work

work :: Integer -> Integer
work n
  | even n = n `div` 2
  | otherwise = 3 * n + 1
