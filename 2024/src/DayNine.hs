module DayNine where

import Data.Maybe (catMaybes, isNothing)
import Data.Char (digitToInt)

data FileBlock = Empty Int | Full Int Int deriving (Show)

expand2 :: [Int] -> [FileBlock]
expand2 = go 0 True
    where
        go _ _ [] = []
        go i True (x : ls) = Full x i : go (i+1) False ls
        go i False (x : ls) = Empty x : go i True ls

fill2 :: [FileBlock] -> [FileBlock]
fill2 = reverse . fillRev . reverse

fillRev :: [FileBlock] -> [FileBlock]
fillRev [] = []
fillRev (Empty l : ls) = Empty l : fillRev ls
fillRev (Full l id : ls) = maybe (Full l id : fillRev ls) go inserted
    where
        inserted = insertBlock (Full l id) (reverse ls)
        go newFB = Empty l : fillRev (reverse newFB)

insertBlock :: FileBlock -> [FileBlock] -> Maybe [FileBlock]
insertBlock (Empty _) _ = Nothing
insertBlock _ [] = Nothing
insertBlock x (Full y z : ls) =
    if isNothing subInsert
        then Nothing
        else Just [Full y z] <> subInsert
    where subInsert = insertBlock x ls
insertBlock (Full l1 id) (Empty l2 : ls)
  | l1 <= l2 = Just [Full l1 id, Empty (l2 - l1)] <> Just ls
  | isNothing subInsert = Nothing
  | otherwise = Just [Empty l2] <> subInsert
  where
      subInsert = insertBlock (Full l1 id) ls

p2 :: [String] -> String
p2 = show . checksum2 . fill2 . expand2 . fmap digitToInt . head

expand :: [Int] -> [Maybe Int]
expand = fmap (fmap (`quot` 2)) . go 0
    where
        oddMaybe i = if even i then Just else const Nothing
        go _ [] = []
        go id (x : ls) = replicate x (oddMaybe id id) ++ go (id+1) ls

expandAndFill :: [Int] -> [Int]
expandAndFill ls = fill expansion (reverse expansion)
    where expansion = expand ls

fill :: [Maybe Int] -> [Maybe Int] -> [Int]
fill [] _ = []
fill _ [] = []
fill ((Just x) : ls) ls2 = x : fill ls ls2
fill (Nothing : ls) (Just x : ls2) = x : fill ls ls2
fill ls (Nothing : ls2) = fill ls ls2

fillWithReverse :: [Maybe Int] -> [Int]
fillWithReverse ls = take total $ fill ls (reverse ls)
    where total = length (catMaybes ls)

checksum :: [Int] -> Int
checksum ls = sum $ uncurry (*) <$> enum
    where enum = zip [0..] ls

reexpand :: [FileBlock] -> [Int]
reexpand [] = []
reexpand (Empty l : ls) = replicate l 0 ++ reexpand ls
reexpand (Full l id : ls) = replicate l id ++ reexpand ls

checksum2 :: [FileBlock] -> Int
checksum2 = checksum . reexpand

p1 :: [String] -> String
p1 = show . checksum . fillWithReverse . expand . fmap digitToInt . head

dayNine :: [String] -> (String, String)
dayNine strs = (p1 strs, p2 strs)