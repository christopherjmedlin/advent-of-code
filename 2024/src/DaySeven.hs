module DaySeven where

import Control.Monad
import Util (lengthGreaterThan1)
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)
import Parsing (integer)
import Data.Either (fromRight)

-- folds over a list using a list of binary operations
foldLBops :: [a] -> [a -> a -> a] -> Maybe a
foldLBops [] _  = Nothing
foldLBops [x] _ = Nothing
foldLBops _ []  = Nothing
foldLBops (y : ls) bops = Just $ foldLBopsHelper ls bops y

foldLBopsHelper :: [a] -> [a -> a -> a] -> a -> a
foldLBopsHelper [] _ x  = x
foldLBopsHelper [a] [b] x = x `b` a
foldLBopsHelper (a : ls) (b : bops) x = foldLBopsHelper ls bops (x `b` a)

mapSamples :: [a] -> Int -> ([a] -> b) -> [b]
mapSamples ls n f = f <$> replicateM n ls

concatInts :: Integer -> Integer -> Integer
concatInts i1 i2 = read (show i1 ++ show i2)

ops1 :: [Integer -> Integer -> Integer]
ops1 = [(+), (*)]

ops2 :: [Integer -> Integer -> Integer]
ops2 = [(+), (*), concatInts]

getAllOpsSequences :: [a] -> [a -> a -> a] -> [[a -> a -> a]]
getAllOpsSequences [] ops = []
getAllOpsSequences ls ops = replicateM (length ls -1) ops

getAllOpSequenceResults :: [Integer -> Integer -> Integer] -> [Integer] -> [Integer]
getAllOpSequenceResults ops ls = mapMaybe (foldLBops ls) (getAllOpsSequences ls ops)

validOpSequenceResults :: [Integer -> Integer -> Integer] -> Integer -> [Integer] -> Bool
validOpSequenceResults ops n = elem n . getAllOpSequenceResults ops

equationParser :: Parser (Integer, [Integer])
equationParser = do
    i1 <- integer
    char ':'
    is <- many1 (char ' ' >> integer)
    return (i1, is)

parseEquations :: [String] -> [(Integer, [Integer])]
parseEquations ls = fromRight (0, []) . parse equationParser "" <$> ls

totalValidOpSequenceResults1 :: [(Integer, [Integer])] -> Integer
totalValidOpSequenceResults1 = sum . fmap fst . filter (uncurry $ validOpSequenceResults ops1)

totalValidOpSequenceResults2 :: [(Integer, [Integer])] -> Integer
totalValidOpSequenceResults2 = sum . fmap fst . filter (uncurry $ validOpSequenceResults ops2)

p1 :: [String] -> String
p1 = show . totalValidOpSequenceResults1 . parseEquations

p2 :: [String] -> String
p2 = show . totalValidOpSequenceResults2 . parseEquations

daySeven :: [String] -> (String, String)
daySeven strs = (p1 strs, p2 strs)