{-# LANGUAGE TupleSections #-}

module DayFour where
import Data.Array
import Data.Bifunctor

matrix :: [[a]] -> Array (Int, Int) a
matrix ls = listArray ((0, 0), (l1 -1, l2 -1))  (concat ls)
    where
        l1 = length ls
        l2 = length (head ls)

-- walks n steps in the given direction from the starting position
-- a step means applying the given increment once
walk :: (Int, Int) -> Integer -> (Int, Int) -> [(Int, Int)]
walk _ 0 _ = []
walk (i1, i2) n (j1, j2) = (i1, i2) : walk (i1+j1, i2+j2) (n-1) (j1, j2)

-- exclusive
inBounds :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
inBounds (x1, x2) ((i1, i2), (j1, j2)) = x1 >= i1 && x1 <= j1 && x2 >= i2 && x2 <= j2

walkAllDirections :: (Int, Int) -> Integer -> [[(Int, Int)]]
walkAllDirections tup n = walk tup n <$> directions
    where directions = [(1,0), (0,1), (-1, 0), (0, -1), (1,1), (-1,-1), (1,-1), (-1,1)]

allPossibleWalks :: Array (Int, Int) a -> Integer -> [[(Int, Int)]]
allPossibleWalks arr n = indices arr >>= flip walkAllDirections n

allPossibleWalksBounded :: Array (Int, Int) a -> Integer -> [[(Int, Int)]]
allPossibleWalksBounded arr n = filter (`inBounds` bounds arr) <$> allPossibleWalks arr n

getStrings :: Array (Int, Int) Char -> [[(Int, Int)]] -> [String]
getStrings arr = fmap (fmap (arr !))

cross1 :: Array (Int, Int) Char -> (Int, Int) -> String
cross1 arr (i1, i2) = (arr !) <$> filter (`inBounds` bounds arr) [(i1-1, i2-1), (i1, i2), (i1+1, i2+1)]

cross2 :: Array (Int, Int) Char -> (Int, Int) -> String
cross2 arr (i1, i2) = (arr !) <$> filter (`inBounds` bounds arr) [(i1-1, i2+1), (i1, i2), (i1+1, i2-1)]

cross :: Array (Int, Int) Char -> (Int, Int) -> (String, String)
cross arr (i1, i2) = bimap (cross1 arr) (cross2 arr) ((i1, i2), (i1, i2))

getAllCrosses :: Array (Int, Int) Char -> [(String, String)]
getAllCrosses arr = cross arr <$> indices arr

isMas :: String -> Bool
isMas str = (str == "MAS") || (str == "SAM")

xmas :: (String, String) -> Bool
xmas (s1, s2) = isMas s1 && isMas s2

allXmas :: Array (Int, Int) Char -> [(String, String)]
allXmas = filter xmas . getAllCrosses

p2 :: [String] -> String
p2 strs = show $ length $ allXmas $ matrix strs

p1 :: [String] -> String
p1 strs = show $ length $ filter (== "XMAS") $ getStrings mat (allPossibleWalksBounded mat 4)
    where mat = matrix strs

dayFour :: [String] -> (String, String)
dayFour ls = (p1 ls, p2 ls)