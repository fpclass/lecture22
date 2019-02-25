--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 21: Sequential composition (cont.)                                 --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Writer where

--------------------------------------------------------------------------------
-- Writer type and instances

data Writer w a = MkWriter (a,w)
    deriving Functor

instance Monoid w => Applicative (Writer w) where
    pure x = MkWriter (x, mempty)

    MkWriter (f,w) <*> MkWriter (x,w') = MkWriter (f x, w `mappend` w')

tell :: w -> Writer w ()
tell w = MkWriter ((),w)

--------------------------------------------------------------------------------
-- Compiler logging example

data Expr = Val Int | Plus Expr Expr
data Instr = PUSH Int | ADD
type Program = [Instr]

data LogMessage = LogM String String

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

--------------------------------------------------------------------------------
