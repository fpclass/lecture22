--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Sequential composition (cont.)                                    --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module State (
    module System.Random,

    State(..),

    randomNumber,
    greetings,
    followUp,
    chooseGreeting,
    sBind,
    chooseGreeting',
    get,
    put,
    fresh
) where

--------------------------------------------------------------------------------

import System.Random (StdGen, random, mkStdGen)

--------------------------------------------------------------------------------
-- State type and instances

data State s a = St { runState :: s -> (a,s) }
    deriving Functor

instance Applicative (State s) where
    pure x = St (\s -> (x,s))

    St m <*> St m' = St $ \s -> let (f,s') = m s
                                    (x,s'') = m' s'
                                in (f x,s'')

runRandom :: State StdGen a -> Int -> (a, StdGen)
runRandom m n = runState m (mkStdGen n)

--------------------------------------------------------------------------------
-- Random number example

randomNumber :: State StdGen Int
randomNumber = St random

greetings :: [String]
greetings = ["Hello there!", "General Kenobi!", "MrPorky!"]

followUp :: [String]
followUp = ["You are a bold one!", "You are a weeb!", "You are a wizard!"]

chooseGreeting :: State StdGen String
chooseGreeting = choose <$> randomNumber <*> randomNumber
    where choose n m = greetings !! (n `mod` 3) ++ " " ++
                       followUp !! (m `mod` 3)

--------------------------------------------------------------------------------
-- Random number example with bind

sBind :: State s a -> (a -> State s b) -> State s b
sBind (St m) f = St $ \s -> let (r,s')  = m s
                                (St m') = f r
                            in m' s'

chooseGreeting' :: State StdGen String
chooseGreeting' = randomNumber `sBind` \n ->
                  randomNumber `sBind` \m ->
                  pure $ greetings !! (n `mod` 3) ++ " " ++
                         followUp !! (m `mod` 3)

--------------------------------------------------------------------------------
-- State primitives

get :: State s s
get = St $ \s -> (s,s)

put :: s -> State s ()
put s = St $ \_ -> ((),s)

--------------------------------------------------------------------------------
-- Using state primitives

fresh :: State Int Int
fresh = get `sBind` \n ->
        put (n+1) `sBind` \_ ->
        pure n

--------------------------------------------------------------------------------
