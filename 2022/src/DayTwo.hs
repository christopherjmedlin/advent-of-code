module DayTwo
    ( day_two ) where

import Data.List
import Control.Monad.State.Lazy

data Shape = Rock | Paper | Scissors

outcome :: Shape -> Shape -> Integer
outcome Rock Rock = 3
outcome Paper Paper = 3
outcome Scissors Scissors = 3
outcome Rock Paper = 6
outcome Rock Scissors = 0
outcome Paper Rock = 0
outcome Paper Scissors = 6
outcome Scissors Rock = 6
outcome Scissors Paper = 0

shapeScore :: Shape -> Integer
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

charToShape :: Char -> Shape
charToShape 'X' = Rock
charToShape 'A' = Rock
charToShape 'Y' = Paper
charToShape 'B' = Paper
charToShape 'Z' = Scissors
charToShape 'C' = Scissors

outcome' :: Shape -> Shape -> Integer
outcome' Rock Rock = 3
outcome' Paper Paper = 2
outcome' Scissors Scissors = 1
outcome' Rock Paper = 1
outcome' Rock Scissors = 2
outcome' Paper Rock = 1
outcome' Paper Scissors = 3
outcome' Scissors Rock = 2
outcome' Scissors Paper = 3

shapeScore' :: Shape -> Integer
shapeScore' Rock = 0
shapeScore' Paper = 3
shapeScore' Scissors = 6

lineToState :: String -> State Integer ()
lineToState str = do
    let w = words str
    let c1 = head (head w)
    let c2 = head (head (tail w))
    let s1 = charToShape c1
    let s2 = charToShape c2
    i <- get 
    put (i + (outcome s1 s2) + (shapeScore s2))

lineToState' :: String -> State Integer ()
lineToState' str = do
    let w = words str
    let c1 = head (head w)
    let c2 = head (head (tail w))
    let s1 = charToShape c1
    let s2 = charToShape c2
    i <- get 
    put (i + (outcome' s1 s2) + (shapeScore' s2))

p1 :: [String] -> Integer
p1 ls = execState (sequence_ (fmap lineToState ls)) 0

p2 :: [String] -> Integer
p2 ls = execState (sequence_ (fmap lineToState' ls)) 0

day_two :: [String] -> (String, String)
day_two x = (show (p1 x), show (p2 x))
