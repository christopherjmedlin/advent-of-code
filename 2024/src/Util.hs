module Util (matrix, inBounds, lengthGreaterThan1) where
import Data.Array (Array)
import Data.Array.Base (listArray)

matrix :: [[a]] -> Array (Int, Int) a
matrix ls = listArray ((0, 0), (l1 -1, l2 -1))  (concat ls)
    where
        l1 = length ls
        l2 = length (head ls)

inBounds :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
inBounds (x1, x2) ((i1, i2), (j1, j2)) = x1 >= i1 && x1 <= j1 && x2 >= i2 && x2 <= j2

lengthGreaterThan1 :: [Integer] -> Bool
lengthGreaterThan1 [] = False
lengthGreaterThan1 [x] = False
lengthGreaterThan1 _ = True