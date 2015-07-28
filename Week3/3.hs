-- histogram [1,1,1,5] ==
--   *
--   *
--   *   *
--  ==========
--  0123456789
--
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
--
--      *
--      *
--      * *
--   ******  *
--  ==========
--  0123456789

main = putStrLn $ histogram [1,4,5,4,6,6,3,4,2,4,9]
--main = putStrLn "$ histogram [1,4,5,4,6,6,3,4,2,4,9]"
--
histogram :: [Int] -> String
histogram [] = ""
histogram a = (concat . map (starOrSpaceRow 0 a) $ reverse a) ++ "==========\n0123456789"

-- process list index
--   | index <= 0 = "==========\n0123456789"
--   | otherwise = starOrSpaceRow 0 list index ++ process list (index - 1)

starOrSpaceRow :: Int -> [Int] -> Int -> String
starOrSpaceRow currentNumber list matchingCount
  | currentNumber > 10 = "\n"
  | matchingCount ==  length (filter (== currentNumber) list) = '*' : (starOrSpaceRow (currentNumber + 1) list matchingCount)
  | otherwise = ' ' : (starOrSpaceRow (currentNumber + 1) list matchingCount)

-- histogram2 xs = let counts = map (length . (flip filter $ xs) . (==)) [0..9] -- get the totals
--                     height = foldl1 max counts -- determine the highest number
--                     lines = map (\x -> map (display . (>= x)) counts) [height, height - 1 .. 1]
--                 in  unlines $ lines ++ [replicate 10 '='] ++ [concatMap show [0..9]]
--
-- display :: Bool -> Char
-- display True  = '*'
-- display False = ' '


-- histogram [] = ""
-- histogram (x:xs) = (take x (repeat ' ')) ++ "*\n" ++ histogram xs
