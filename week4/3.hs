
-- xor [False, True, False] == True
--
-- xor [False, True, False, False, True] == False

xor :: [Bool] -> Bool
xor = odd . length . filter (== True)

-- xor' :: [Bool] -> Bool
-- xor' = foldl rec False where
--   rec True False = True
--   rec False True = True
--   rec _ _ = False


map’ :: (a -> b) -> [a] -> [b]
map’ f a = foldl1 f
