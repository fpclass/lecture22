--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Sequential composition (cont.)                                    --
--------------------------------------------------------------------------------

module Do where

--------------------------------------------------------------------------------
-- do-notation examples

safediv :: Int -> Int -> Maybe Int
safediv x 0 = Nothing
safediv x y = Just (x `div` y)

data Expr = Val Int
          | Add Expr Expr
          | Div Expr Expr
          | If Expr Expr Expr

eval :: Expr -> Maybe Int
eval (Val n)   = return n
eval (Add l r) = do
    x <- eval l
    y <- eval r
    return (x+y)
eval (Div l r) = do
    x <- eval l
    y <- eval r
    x `safediv` y
eval (If c t f) = do
    b <- eval c

    if b /= 0
    then eval t
    else eval f

e0 :: Expr
e0 = Div (Val 8) (Val 2)

e1 :: Expr
e1 = Div (Val 8) (Val 0)

e2 :: Expr
e2 = If (Val 1) (Val 2) (Val 3)

e3 :: Expr
e3 = If (Val 0) (Val 2) (Val 3)

e4 :: Expr
e4 = If (Val 1) (Val 2) e1

--------------------------------------------------------------------------------
