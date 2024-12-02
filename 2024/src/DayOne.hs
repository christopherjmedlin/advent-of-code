module DayOne
    ( dayOne ) where

import Data.List
import Data.Bifunctor
import Text.Parsec

distance :: (Integer, Integer) -> Integer
distance (i1, i2) = abs $ i2 - i1

p1Sorted :: [(Integer, Integer)] -> Integer
p1Sorted = sum . fmap distance

p1Ints :: [(Integer, Integer)] -> Integer
p1Ints = p1Sorted . sortTuple

sortTuple :: [(Integer, Integer)] -> [(Integer, Integer)]
sortTuple = join . bimap sort sort . split

split :: [(Integer, Integer)] -> ([Integer], [Integer])
split [] = ([], [])
split ((i1, i2) : ls) = (i1 : fst newLs, i2 : snd newLs)
    where newLs = split ls

join :: ([Integer], [Integer]) -> [(Integer, Integer)]
join ([], _) = []
join (_, []) = []
join (i1 : ls1, i2 : ls2) = (i1, i2) : join (ls1, ls2)

parseLineLs :: String -> [Integer]
parseLineLs str = map read $ words str

parseLine :: String -> (Integer, Integer)
parseLine str = (parseLineLs str !! 0, parseLineLs str !! 1)

parseDay1 :: [String] -> [(Integer, Integer)]
parseDay1 = fmap parseLine

p1 :: [String] -> String
p1 = show . p1Ints . parseDay1

dayOne :: [String] -> (String, String)
dayOne ls = (p1 ls, "")