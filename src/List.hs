--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Sequential composition (cont.)                                    --
--------------------------------------------------------------------------------

module List where

--------------------------------------------------------------------------------

coords :: (Num a, Enum a) => [(a,a)]
coords = [(x,y) | x <- [0..10], y <- [0..10]]

lBind :: [a] -> (a -> [b]) -> [b]
lBind xs f = concat (map f xs)

coords' :: (Num a, Enum a) => [(a,a)]
coords' = [0..10] `lBind` \x ->
          [0..10] `lBind` \y ->
          pure (x,y)

evens :: [Int]
evens = [n | n <- [0..100], even n]

evens' :: [Int]
evens' = undefined 

--------------------------------------------------------------------------------
