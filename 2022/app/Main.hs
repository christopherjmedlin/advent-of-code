module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if (null args)
        then putStrLn "Usage: aoc2022 <day>" 
        else return ()
    let f = (getDay.read.head) args
    s <- readFile ("./input/day" ++ head args)
    let (p1, p2) = f (lines s)
    putStrLn $ "Part 1: " ++ p1
    putStrLn $ "Part 2: " ++ p2
