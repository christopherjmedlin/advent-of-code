module DayOne
    ( day_one ) where

import Data.List

split :: [String] -> [[String]]
split = go []
    where go acc ("" : xs) = [acc] ++ split xs
          go acc (s : xs)  = go (acc ++ [s]) xs
          go acc [] = [acc]

nums = ((fmap.fmap) read)
sums = fmap sum
maxls = foldr max 0

p1 = (show.maxls.sums.nums.split)

top3 :: [Integer] -> (Integer, Integer, Integer)
top3 ls = (one, two, three)
    where one = maxls ls
          two = maxls (delete one ls)
          three = maxls (delete two (delete one ls))

sumTup (x,y,z) = x + y + z

p2 = (show.sumTup.top3.sums.nums.split)

day_one :: [String] -> (String, String)
day_one ls = (p1 ls, p2 ls)
