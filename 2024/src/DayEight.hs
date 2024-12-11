module DayEight where

import Prelude as P
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Set (Set, fromList, toList, empty, insert, unions, size)
import Data.Set as M
import Util (matrix, inBounds, inBoundsInteger)
import Data.Array ((!), indices)
import Data.List (groupBy)
import Data.List (sortBy)

type Coordinate = (Integer, Integer)

addCoords :: Coordinate -> Coordinate -> Coordinate
addCoords (i1, i2) (j1, j2) = (i1 + j1, i2 + j2)

inverse :: Coordinate -> Coordinate
inverse = bimap (* (-1)) (* (-1))

diff :: Coordinate -> Coordinate -> Coordinate
diff c1 c2 = addCoords c1 (inverse c2)

antinodes :: Coordinate -> Coordinate -> (Coordinate, Coordinate)
antinodes c1 c2 = (first, second)
    where
        dist1 = diff c1 c2
        dist2 = inverse dist1
        first = addCoords dist1 c1
        second = addCoords dist2 c2

antinodes2 :: Coordinate -> Coordinate -> (Coordinate, Coordinate)
antinodes2

tupFlatMap :: (b -> (a, a)) -> [b] -> [a]
tupFlatMap f [] = []
tupFlatMap f (x : ls) = x1 : (x2 : tupFlatMap f ls)
    where (x1, x2) = f x

pairs :: [a] -> [(a, a)]
pairs ls = do
    x1 <- ls
    x2 <- ls
    return (x1, x2)

uniquePairs :: Eq b => [b] -> [(b, b)]
uniquePairs = P.filter (uncurry (/=)) . pairs

allAntinodes :: Set Coordinate -> Set Coordinate
allAntinodes coords = fromList $ tupFlatMap (uncurry antinodes) $ p
    where p = uniquePairs $ toList coords

parseAntennas :: [String] -> [Set Coordinate]
parseAntennas strs = fromList . fmap (bimap toInteger toInteger) <$> groups
    where
        mat = matrix strs
        antennaIx = P.filter (\x -> mat ! x /= '.') $ indices mat
        arrEq arr i j = arr ! i == arr ! j
        arrSort arr i j = compare (arr ! i) (arr ! j)
        groups = groupBy (arrEq mat) (sortBy (arrSort mat) antennaIx)

allAntinodesInFile :: [String] -> Set Coordinate
allAntinodesInFile = unions . fmap allAntinodes . parseAntennas

allAntinodesInBounds :: [String] -> Set Coordinate
allAntinodesInBounds ls = M.filter (`inBoundsInteger` bounds) . allAntinodesInFile $ ls
    where
        x1 = length ls
        x2 = length $ head ls
        bounds = ((0, 0), (toInteger x1 - 1, toInteger x2 - 1))

p1 :: [String] -> String
p1 = show . size . allAntinodesInBounds

dayEight :: [String] -> (String, String)
dayEight strs = (p1 strs, "")