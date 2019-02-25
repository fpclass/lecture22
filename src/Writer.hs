--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 21: Sequential composition (cont.)                                 --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Writer where

--------------------------------------------------------------------------------
-- Writer type and instances

data Writer w a = MkWriter { runWriter :: (a,w) }
    deriving (Show, Functor)

instance Monoid w => Applicative (Writer w) where
    pure x = MkWriter (x, mempty)

    MkWriter (f,w) <*> MkWriter (x,w') = MkWriter (f x, w `mappend` w')

tell :: w -> Writer w ()
tell w = MkWriter ((),w)

--------------------------------------------------------------------------------
-- Compiler logging example

data Expr = Val Int | Plus Expr Expr deriving Show
data Instr = PUSH Int | ADD deriving Show
type Program = [Instr]

data LogMessage = LogM String String deriving Show

logM :: String -> String -> Writer [LogMessage] ()
logM source message = tell [LogM source message]

comp :: Expr -> Writer [LogMessage] Program
comp (Val n)    = logM "comp" "compiling a value" *> pure [PUSH n]
comp (Plus l r) = logM "comp" "compiling a plus" *>
    ((\p p' -> p ++ p' ++ [ADD]) <$> comp l <*> comp r)

--------------------------------------------------------------------------------
-- Compiler logging example with bind

wBind :: Monoid w => Writer w a -> (a -> Writer w b) -> Writer w b
wBind (MkWriter (x,w)) f = MkWriter $ let (MkWriter (y,w')) = f x
                                      in (y, w `mappend` w')

comp' :: Expr -> Writer [LogMessage] Program
comp' (Val n)    = logM "comp" "compiling a value" `wBind` \_ ->
                   pure [PUSH n]
comp' (Plus l r) = logM "comp" "compiling a plus" `wBind` \_ ->
                   comp l `wBind` \p ->
                   comp r `wBind` \p' ->
                   pure (p ++ p' ++ [ADD])

w0 :: Expr
w0 = Val 4

w1 :: Expr
w1 = Plus (Val 4) (Val 8)

w2 :: Expr
w2 = Plus w1 w1

--------------------------------------------------------------------------------
-- super forbidden knowledge

runWriterPretty :: Show a => Writer [LogMessage] a -> IO ()
runWriterPretty w = let (r,o) = runWriter w in do
    print r
    mapM_ print o

--------------------------------------------------------------------------------
