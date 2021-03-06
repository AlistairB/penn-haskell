module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

-- let st’ = extend st "A" 5
-- in st’ "A" == 5

extend :: State -> String -> Int -> State
extend state key value = f
  where
    f s
      | s == key = value
      | otherwise = state key

empty :: State
empty = \_ -> 0

-- Exercise 2 -----------------------------------------

-- evalE empty (Val 5) == 5
--
-- evalE empty (Op (Val 1) Eql (Val 2)) == 0

evalE :: State -> Expression -> Int
evalE state (Var x) = state x
evalE state (Val x) = x
evalE state (Op expr1 bop expr2) = bopProcess bop (evalE state expr1) (evalE state expr2)
  where
    bopProcess bop int1 int2
      | bop == Plus = int1 + int2
      | bop == Minus = int1 - int2
      | bop == Times = (fromIntegral $ int1 * int2)
      | bop == Divide = (fromIntegral $ round $ (realToFrac int1) / (realToFrac int2))
      | bop == Gt = if int1 > int2 then 1 else 0
      | bop == Ge = if int1 >= int2 then 1 else 0
      | bop == Lt = if int1 < int2 then 1 else 0
      | bop == Le = if int1 <= int2 then 1 else 0
      | bop == Eql = if int1 == int2 then 1 else 0

--
-- -- Exercise 3 -----------------------------------------
--
-- data DietStatement = DAssign String Expression
--                    | DIf Expression DietStatement DietStatement
--                    | DWhile Expression DietStatement
--                    | DSequence DietStatement DietStatement
--                    | DSkip
--                      deriving (Show, Eq)
--
-- desugar :: Statement -> DietStatement
-- desugar = undefined
--
--
-- -- Exercise 4 -----------------------------------------
--
-- evalSimple :: State -> DietStatement -> State
-- evalSimple = undefined
--
-- run :: State -> Statement -> State
-- run = undefined
--
-- -- Programs -------------------------------------------
--
-- slist :: [Statement] -> Statement
-- slist [] = Skip
-- slist l  = foldr1 Sequence l
--
-- {- Calculate the factorial of the input
--
--    for (Out := 1; In > 0; In := In - 1) {
--      Out := In * Out
--    }
-- -}
-- factorial :: Statement
-- factorial = For (Assign "Out" (Val 1))
--                 (Op (Var "In") Gt (Val 0))
--                 (Assign "In" (Op (Var "In") Minus (Val 1)))
--                 (Assign "Out" (Op (Var "In") Times (Var "Out")))
--
--
-- {- Calculate the floor of the square root of the input
--
--    B := 0;
--    while (A >= B * B) {
--      B++
--    };
--    B := B - 1
-- -}
-- squareRoot :: Statement
-- squareRoot = slist [ Assign "B" (Val 0)
--                    , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
--                        (Incr "B")
--                    , Assign "B" (Op (Var "B") Minus (Val 1))
--                    ]
--
-- {- Calculate the nth Fibonacci number
--
--    F0 := 1;
--    F1 := 1;
--    if (In == 0) {
--      Out := F0
--    } else {
--      if (In == 1) {
--        Out := F1
--      } else {
--        for (C := 2; C <= In; C++) {
--          T  := F0 + F1;
--          F0 := F1;
--          F1 := T;
--          Out := T
--        }
--      }
--    }
-- -}
-- fibonacci :: Statement
-- fibonacci = slist [ Assign "F0" (Val 1)
--                   , Assign "F1" (Val 1)
--                   , If (Op (Var "In") Eql (Val 0))
--                        (Assign "Out" (Var "F0"))
--                        (If (Op (Var "In") Eql (Val 1))
--                            (Assign "Out" (Var "F1"))
--                            (For (Assign "C" (Val 2))
--                                 (Op (Var "C") Le (Var "In"))
--                                 (Incr "C")
--                                 (slist
--                                  [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
--                                  , Assign "F0" (Var "F1")
--                                  , Assign "F1" (Var "T")
--                                  , Assign "Out" (Var "T")
--                                  ])
--                            )
--                        )
--                   ]
