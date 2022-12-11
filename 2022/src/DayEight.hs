module DayEight
    (day_eight) where

import Data.Array

lulzToArr :: [[a]] -> Array (Int, Int) a
lulzToArr ls = array bs $ prod `zip` concat ls
    where l1 = [0..length ls - 1]
          l2 = [0..length (head ls) - 1]
          prod = (,) <$> l1 <*> l2
          bs = ((0,0),((length ls)-1, (length (head ls)-1)))

allIndices :: Ix a => (b -> Bool) -> Array a b -> [a] -> Bool
allIndices p arr = all (p . ((!) arr))

upsDownsLeftsRights :: Array (Int, Int) Int -> (Int, Int) -> [[(Int, Int)]]
upsDownsLeftsRights arr (i,j) = [ups, downs, lefts, rights]
    where (rowB, colB) = snd $ bounds arr
          ups = reverse [(x,j) | x <- [0..i-1]]
          downs = [(x,j) | x <- [i+1..rowB]]
          lefts = reverse [(i,x) | x <- [0..j-1]]
          rights = [(i,x) | x <- [j+1..colB]]

isVisible :: Array (Int, Int) Int -> (Int, Int) -> Bool
isVisible arr (i, j) = or $ p <$> upsDownsLeftsRights arr (i,j)
    where p = allIndices (\x -> x < arr ! (i,j)) arr 

numVisible :: Array (Int, Int) Int -> Int
numVisible arr = length $ filter (isVisible arr) (indices arr)
    where (rowB, colB) = snd $ bounds arr

-- including first element that doesn't satisfy
takeWhileInc :: (a -> Bool) -> [a] -> [a]
takeWhileInc p [] = []
takeWhileInc p (x:xs)
    | p x = x : takeWhileInc p xs
    | otherwise = [x]

scenicScore :: Array (Int, Int) Int -> (Int, Int) -> Int
scenicScore arr (i,j) = foldr (*) 1 $ f <$> upsDownsLeftsRights arr (i,j)
    where p ix = (arr ! ix) < (arr ! (i,j))
          f = length . (takeWhileInc p)

maxScenicScore :: Array (Int, Int) Int -> Int
maxScenicScore arr = foldr max 0 $ (scenicScore arr) <$> (indices arr)

parse :: [String] -> Array (Int, Int) Int
parse = lulzToArr . ((fmap.fmap) (read.pure))

p1 = show . numVisible . parse
p2 = show . maxScenicScore . parse

day_eight lines = (p1 lines, p2 lines)
