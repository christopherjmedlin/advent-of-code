module DayTwo where

import Data.Monoid
import Text.Parsec (parse)
import Parsing ( parseListOfInteger )
import Data.List (sort)

possibleRemovals :: [Integer] -> [[Integer]]
possibleRemovals [] = []
possibleRemovals (i : ls) = ls : map (i :) (possibleRemovals ls)

safe2 :: [Integer] -> Bool
safe2 = any safe . possibleRemovals

safe :: [Integer] -> Bool
safe ls = (helper . pairs) ls || (helper . fmap switchPair . pairs) ls
    where
        helper = getAll . foldMap (All . validLevels)

validLevels :: (Integer, Integer) -> Bool
validLevels (i1, i2) = i2 - i1 >= 1 && i2 - i1 <= 3

switchPair :: (Integer, Integer) -> (Integer, Integer)
switchPair (x, y) = (y, x)

pairs :: [Integer] -> [(Integer, Integer)]
pairs [] = []
pairs [i1] = []
pairs (i1 : (i2 : ls)) = (i1, i2) : pairs (i2 : ls)

parseInts :: String -> [Integer]
parseInts = parseListOfInteger

p1 :: [String] -> String
p1 = show . length . filter safe . fmap parseInts

p2 :: [String] -> String
p2 = show . length . filter safe2 . fmap parseInts

dayTwo :: [String] -> (String, String)
dayTwo ls = (p1 ls, p2 ls)