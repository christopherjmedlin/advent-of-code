module DayThree
    ( day_three ) where

import Data.List
import Data.Char

compartments :: String -> (String, String)
compartments s = (take l s, drop l s)
    where l = quot (length s) 2

inList ls x = elem x ls

common :: (String, String) -> Char
common (ls1, ls2) = head (filter (inList ls2) ls1)

val c
    | c < 'a' = (ord c) - (ord 'A') + 27
    | otherwise = (ord c) - (ord 'a') + 1

threes :: [String] -> [[String]]
threes [] = []
threes ls = ((take 3) ls) : (threes ((drop 3) ls))

inAllThree :: [String] -> Char
inAllThree [ls1, ls2, ls3] = head (filter (\x -> ((inList ls2 x) && (inList ls3 x))) ls1)

p2 = show . sum . ((fmap (val.inAllThree)).threes)
p1 = show . sum . (fmap (val.common.compartments))

day_three :: [String] -> (String, String)
day_three ls = (p1 ls, p2 ls)
