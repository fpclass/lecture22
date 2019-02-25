--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 21: Sequential composition (cont.)                                 --
--------------------------------------------------------------------------------

-- Needed because we define our own Monad type class rather than the one from
-- the standard library and the do-notation desugars to the standard library
-- functions by default
{-# LANGUAGE RebindableSyntax #-}

module Monad where

--------------------------------------------------------------------------------

import Prelude hiding (Monad(..), mapM)

import List
import Writer
import State

--------------------------------------------------------------------------------

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b

    return :: a -> m a
    return = pure

(>>) :: Monad m => m a -> m b -> m b
m >> m' = m >>= \_ -> m'

instance Monad Maybe where
    Nothing >>= f = Nothing
    Just x  >>= f = f x

instance Monad [] where
    xs >>= f = concat (map f xs)

instance Monoid w => Monad (Writer w) where
    MkWriter (x,w) >>= f = MkWriter $ let MkWriter (r,w') = f x
                                      in (r,w `mappend` w')

instance Monad (State s) where
    St m >>= f = St $ \s -> let (x,s')  = m s
                                (St m') = f x
                            in m' s'

--------------------------------------------------------------------------------
-- Helper functions (ordinarily pre-defined in Control.Monad)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ []     = return []
mapM f (x:xs) =
    f x >>= \r ->
    mapM f xs >>= \rs ->
    return (r:rs)

void :: Functor f => f a -> f ()
void m = (\_ -> ()) <$> m

join :: Monad m => m (m a) -> m a
join m = m >>= id

replicateM :: Applicative f => Int -> f a -> f [a]
replicateM 0 f = pure []
replicateM n f = (:) <$> f <*> replicateM (n-1) f

when :: Applicative f => Bool -> f () -> f ()
when True f  = f
when False _ = pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless b = when (not b)

--------------------------------------------------------------------------------
-- do-notation examples

coords'' :: (Num a, Enum a) => [(a,a)]
coords'' = do
    x <- [0..10]
    y <- [0..10]
    pure (x,y)

comp'' :: Expr -> Writer [LogMessage] Program
comp'' (Val n) = do
    logM "comp" "compiling a value"
    pure [PUSH n]
comp'' (Plus l r) = do
    logM "comp" "compiling a plus"
    p  <- comp l
    p' <- comp r
    pure (p ++ p' ++ [ADD])

--------------------------------------------------------------------------------
