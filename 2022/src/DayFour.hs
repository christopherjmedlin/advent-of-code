module DayFour
    ( day_four ) where

import Data.List
import Data.Char
import Text.Parsec.String
import Text.Parsec
import Data.Either

integer = read <$> many1 digit

parser :: Parser ((Int, Int), (Int, Int))
parser = do
    i1 <- integer
    char '-'
    i2 <- integer
    char ','
    i3 <- integer
    char '-'
    i4 <- integer
    return ((i1, i2), (i3, i4))  

r = (fromRight ((0,0),(0,0))) . (runParser parser () "")

contains :: (Int, Int) -> (Int, Int) -> Bool
contains (x1, y1) (x2, y2) = (x1 <= x2) && (y1 >= y2)

overlap (x1, y1) (x2, y2) = not ((y1 < x2) || (x1 > y2))

p (p1, p2) = (contains p1 p2) || (contains p2 p1)
p' (p1, p2) = (overlap p1 p2) || (overlap p2 p1)

pairs = fmap r

p1 = show.length.(filter p).pairs
p2 = show.length.(filter p').pairs

day_four :: [String] -> (String, String)
day_four ls = (p1 ls, p2 ls)
