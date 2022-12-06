module DaySix
    ( day_six ) where

import Prelude hiding (lookup)
import Data.List hiding (lookup, insert)
import Data.Map.Strict hiding (take)

fromJust _ (Just x) = x
fromJust y Nothing  = y

allUnique :: Ord a => [a] -> Bool
allUnique ls = go ls empty
    where go [] _ = True
          go (x : xs) m = (fromJust True (lookup x m)) &&
                          go xs (insert x False m)

elemsUntil :: [a] -> (a -> Bool) -> Integer
elemsUntil = go 0
    where go acc [] _       = acc
          go acc (x : xs) p
            | p x       = acc
            | otherwise = go (acc+1) xs p

groups :: Integer -> [a] -> [[a]]
groups n ls
    | length ls >= (fromIntegral )n = take (fromIntegral n) ls : 
                                      groups n (tail ls)
    | otherwise      = []

packetMarker :: Integer -> String -> Integer
packetMarker n s = (elemsUntil (groups n s) allUnique) + n

p1 = show . (packetMarker 4) . head
p2 = show . (packetMarker 14) . head

day_six :: [String] -> (String, String)
day_six ls = (p1 ls, p2 ls)
