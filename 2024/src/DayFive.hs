module DayFive where

import Prelude as P
import Data.Set as S
import Data.Map as M
import Data.Bifunctor
import Text.Parsec.String as P
import Parsing (integer, commaSeparatedIntegers)
import Text.Parsec (char, runParser)
import Data.Either (fromRight)
import Data.List (sortBy)

import Util (lengthGreaterThan1)

data OrderingRecord = OrderingRecord {before :: Set Integer, after :: Set Integer} deriving (Show)
type IOrdering = Map Integer OrderingRecord
type Slice = (Set Integer, Integer, Set Integer)
type Slices = [Slice]

addBefore :: Integer -> OrderingRecord -> OrderingRecord
addBefore i record = OrderingRecord { before=S.insert i $ before record, after = after record}

addAfter :: Integer -> OrderingRecord -> OrderingRecord
addAfter i record = OrderingRecord { before=before record, after = S.insert i $ after record}

getBefore :: Integer -> IOrdering -> Set Integer
getBefore i map = before $ (map ! i)

getAfter :: Integer -> IOrdering -> Set Integer
getAfter i map = after $ (map ! i)

newOrderingRecord :: Integer -> OrderingRecord
newOrderingRecord i = OrderingRecord {before=S.empty, after=S.empty}

insertIfNotExists :: Integer -> IOrdering -> IOrdering
insertIfNotExists x map = 
    if not (M.member x map)
        then M.insert x (newOrderingRecord x) map 
        else map

updateOrdering :: IOrdering -> (Integer, Integer) -> IOrdering
updateOrdering m (i1, i2) = updateAfter
    where
        map = insertIfNotExists i1 (insertIfNotExists i2 m)
        updateBefore = M.insert i2 (addBefore i1 $ map ! i2) map
        updateAfter = M.insert i1 (addAfter i2 $ updateBefore ! i1) updateBefore

makeOrdering :: [(Integer, Integer)] -> IOrdering
makeOrdering = P.foldl updateOrdering M.empty

orderParser :: Parser (Integer, Integer)
orderParser = do
    i1 <- integer
    char '|'
    i2 <- integer
    return (i1, i2)

parseOrders :: [String] -> [(Integer, Integer)]
parseOrders = P.filter (\x -> x /= (-1, -1)) . fmap (fromRight (-1, -1) . runParser orderParser () "")

parseOrdering :: [String] -> IOrdering
parseOrdering = makeOrdering . parseOrders

parseSequences :: [String] -> [[Integer]]
parseSequences = fmap (fromRight [] . runParser commaSeparatedIntegers () "")

slices :: [Integer] -> Slices
slices = go []
    where
        go prev [] = []
        go prev [x] = [(S.fromList prev, x, S.empty)]
        go prev (x : lsTail) = go (x : prev) lsTail ++ [(S.fromList prev, x, S.fromList lsTail)]

inOrder :: Slice -> IOrdering -> Bool
inOrder (beforeI, i, afterI) map = S.null afterIntersection && S.null beforeIntersection
    where
        afterO = after $ map ! i
        beforeO = before $ map ! i
        afterIntersection = S.intersection beforeI afterO
        beforeIntersection = S.intersection afterI beforeO

validOrdering :: [Integer] -> IOrdering -> Bool
validOrdering ls ord = all (inOrderF ord) (slices ls)
    where
        inOrderF = flip inOrder

middle :: [a] -> Int
middle = (`div` 2) . length

middleElem :: [a] -> a
middleElem ls = ls !! middle ls

sumMiddleElems :: Num a => [[a]] -> a
sumMiddleElems = sum . fmap middleElem

compareByOrdering :: IOrdering -> Integer -> Integer -> Ordering
compareByOrdering ord x y = f $ M.lookup x ord
    where
        f Nothing = EQ
        f (Just x)  = if S.member y $ before x then GT else LT

sortByOrdering :: IOrdering -> [Integer] -> [Integer]
sortByOrdering ord = sortBy (compareByOrdering ord)

p2 :: [String] -> String
p2 strs = show $ sumMiddleElems $ sortByOrdering ordering <$> filteredSeqs
    where
        ordering = parseOrdering strs
        seqs = P.filter lengthGreaterThan1 $ parseSequences strs
        filteredSeqs = P.filter (not . (`validOrdering` ordering)) seqs

p1 :: [String] -> String
p1 strs = show $ sumMiddleElems $ P.filter (`validOrdering` ordering) seqs
    where
        ordering = parseOrdering strs
        seqs = P.filter lengthGreaterThan1 $ parseSequences strs

dayFive :: [String] -> (String, String)
dayFive ls = (p1 ls, p2 ls)
