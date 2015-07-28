
-- 2 = a c, a b, c b
-- 3 = a b, a c, b c, a b, c a, c b, a b
-- 4 = a c, a b, c b, a c, b a, b c, a c, a b, c b, c a, b a, c b, a c, a b, c b
-- 5 =

type Peg = String
type Move = (Peg, Peg)
-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi 0 _ _ _ = []
-- hanoi 1 o d s = [(o, d)]
-- hanoi 2 o d s = moveTwoStack o d s
-- hanoi n o d s = (generateLeft (n-2) o d s) ++ [("a", "b")] ++ (generateRight (n-2) o d s)
--   where
--     --eitherSideRecursions = n - 2
--     generateLeft r o d s
--        | r == 1 && (n `mod` 2 == 0)   = moveTwoStack o d s
--        | r == 1 && (n `mod` 2 /= 0)   = moveTwoStack o s d
--        | r `mod` 2 == 0 = (generateLeft (r-1) o d s) ++ [(o, s)] ++ (moveTwoStack d s o)
--        | otherwise      = (generateLeft (r-1) o d s) ++ [(d, s)] ++ (moveTwoStack o d s)
--     generateRight r o d s
--       | r == 1 && (n `mod` 2 == 0)   = moveTwoStack o d s
--       | r == 1 && (n `mod` 2 /= 0)   = moveTwoStack s d o
--       | r `mod` 2 == 0 = (moveTwoStack d s o) ++ [(o, s)] ++ (generateRight (r-1) o d s)
--       | otherwise      = (moveTwoStack o d s) ++ [(d, s)] ++ (generateRight (r-1) o d s)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0    = []
  | otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- hanoi n a b c =
--   | n `mod` 2 == 0

moveTwoStack :: Peg -> Peg -> Peg -> [Move]
moveTwoStack o d s = [(o, s), (o, d), (s, d)]

-- er = (moveTwoStack "a" "c" "b") ++ [("a", "b")] ++ (moveTwoStack "c" "b" "a")
--
-- erTwo = (moveTwoStack "a" "b" "c") ++ [("a, c")]
--   ++ (moveTwoStack "b" "c" "a") ++ [("a", "b")]
--   ++ (moveTwoStack "c" "a" "b") ++ [("c", "b")]
--   ++ (moveTwoStack "a" "b" "c")
