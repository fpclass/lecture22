--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Sequential composition (cont.)                                    --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Writer where

--------------------------------------------------------------------------------
-- Writer type and instances

data Writer w a = MkWriter { runWriter :: (a,w) }
    deriving (Show, Functor)

instance Monoid w => Applicative (Writer w) where
    pure x = MkWriter (x, mempty)

    MkWriter (f,o1) <*> MkWriter (x,o2) = MkWriter (f x, o1 <> o2)

--------------------------------------------------------------------------------
-- Compiler logging example

data Expr = Val Int | Plus Expr Expr deriving Show
data Instr = PUSH Int | ADD deriving Show
type Program = [Instr]

writeLog :: String -> Writer [String] ()
writeLog msg = MkWriter ((),[msg])

comp :: Expr -> Writer [String] Program
comp (Val n)    = writeLog "compiling a value" *> pure [PUSH n]
comp (Plus l r) = writeLog "compiling a plus" *>
    ((\pl pr -> pl ++ pr ++ [ADD]) <$> comp l <*> comp r)

--------------------------------------------------------------------------------
-- Compiler logging example with bind

wBind :: Monoid w => Writer w a -> (a -> Writer w b) -> Writer w b
wBind (MkWriter (x,o1)) f = MkWriter $ let (MkWriter (y,o2)) = f x
                                       in (y, o1 <> o2)

comp' :: Expr -> Writer [String] Program
comp' (Val n)    = writeLog "compiling a value" `wBind` \_ ->
                   pure [PUSH n]
comp' (Plus l r) = writeLog "compiling a plus" `wBind` \_ ->
                   comp l `wBind` \pl ->
                   comp r `wBind` \pr ->
                   pure (pl ++ pr ++ [ADD])

w0 :: Expr
w0 = Val 4

w1 :: Expr
w1 = Plus (Val 4) (Val 8)

w2 :: Expr
w2 = Plus w1 w1

--------------------------------------------------------------------------------
-- super forbidden knowledge

runWriterPretty :: Show a => Writer [String] a -> IO ()
runWriterPretty w = let (r,o) = runWriter w in do
    print r
    mapM_ print o

--------------------------------------------------------------------------------
