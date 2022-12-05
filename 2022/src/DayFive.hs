module DayFive
    ( day_five ) where

import Prelude hiding (lookup)
import Data.List hiding (insert, lookup)
import Data.Char
import Data.Map.Strict hiding (filter, take, drop)
import Text.Parsec hiding (State)
import Text.Parsec.String
import Control.Monad.State.Lazy
import Data.Either
import Data.Maybe

parseStack :: [String] -> Integer -> [Char]
parseStack [] i = []
parseStack ls i = ((head ls) !! ind) : (parseStack (tail ls) i)
    where ind = fromIntegral (((i-1) * 4) + 1)

getStacks :: [String] -> [String]
getStacks strs = fmap (parseStack strs) [1..9]

rmSpace = filter (/= ' ')

stackMap :: [String] -> Map Integer [Char]
stackMap strs = fromList ([1..9] `zip` (fmap rmSpace (getStacks strs)))

move :: Integer -> Integer -> Integer -> State (Map Integer [Char]) ()
move 1 src dest = do
    m <- get
    let srcls = fromJust (lookup src m)
    let destls = fromJust (lookup dest m)
    let newm = insert dest ((head srcls) : destls) m
    put $ insert src (tail srcls) newm
move n src dest = do
    move 1 src dest
    move (n - 1) src dest

delAt i ls = take i ls ++ drop (i + 1) ls

move' :: Integer -> Integer -> Integer -> State (Map Integer [Char]) ()
move' 0 src dest = return ()
move' n src dest = do
    m <- get
    let srcls = fromJust (lookup src m)
    let destls = fromJust (lookup dest m)
    let newm = insert dest ((srcls !! ((fromIntegral n)-1)) : destls) m
    put $ insert src (delAt (fromIntegral (n-1)) srcls) newm
    move' (n - 1) src dest

integer = read <$> many1 digit

instrParser :: Parser (State (Map Integer [Char]) ())
instrParser = do
    string "move "
    i1 <- integer
    string " from "
    i2 <- integer
    string " to "
    i3 <- integer
    return $ move i1 i2 i3

instrParser' :: Parser (State (Map Integer [Char]) ())
instrParser' = do
    string "move "
    i1 <- integer
    string " from "
    i2 <- integer
    string " to "
    i3 <- integer
    return $ move' i1 i2 i3

runp :: String -> State (Map Integer [Char]) ()
runp str = fromRight (return ()) (runParser instrParser () "" str)

runp' :: String -> State (Map Integer [Char]) ()
runp' str = fromRight (return ()) (runParser instrParser' () "" str)

runSimulation :: [String] -> Map Integer [Char] -> Map Integer [Char]
runSimulation strs init = execState ((sequence.(fmap runp)) strs) init

runSimulation' :: [String] -> Map Integer [Char] -> Map Integer [Char]
runSimulation' strs init = execState ((sequence.(fmap runp')) strs) init

tops :: Map Integer [Char] -> String
tops m = fmap f [1..9]
    where f x = head.fromJust $ lookup x m

p1 lines = tops (runSimulation (drop 10 lines) (stackMap (take 8 lines)))
p2 lines = tops (runSimulation' (drop 10 lines) (stackMap (take 8 lines)))

day_five :: [String] -> (String, String)
day_five ls = (p1 ls, p2 ls)
