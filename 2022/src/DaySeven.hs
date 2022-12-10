module DaySeven
    (day_seven) where

import Control.Monad.State.Lazy
import Text.Parsec.String
import Text.Parsec hiding (State, label)
import Data.Either
import Data.Tree
import Data.Tree.Zipper hiding (last)
import Data.Maybe
import Data.List hiding (insert, group)

data File = Directory String | Entry String Integer deriving (Eq, Show)
data Com = CdChild String | CdParent | Ls [File] deriving Show

startsWith :: Eq a => a -> [a] -> Bool
startsWith _ [] = False
startsWith y (x : _) = x == y

group :: [a] -> (a -> Bool) -> [[a]]
group [] p = []
group (x:xs) p = go xs [x]
    where go [] acc = [acc]
          go (y : ys) acc
            | p y = [acc] ++ go ys [y]
            | otherwise = go ys (acc ++ [y])

integer :: Parser Integer
integer = read <$> many1 digit

fileParser :: Parser File
fileParser = do
    m <- optionMaybe integer
    case m of
        Just i -> do
            char ' '
            s <- many anyChar
            return $ Entry s i
        Nothing -> do
            string "dir "
            s <- many anyChar
            return $ Directory s

parseFile :: String -> File
parseFile = (fromRight (Directory "")) . (runParser fileParser () "")

parseComGroup :: [String] -> Com
parseComGroup ls
    | (head ls !! 2) == 'l' = Ls $ 
            fmap parseFile (drop 1 ls)
    | (head ls !! 5) == '.' = CdParent
    | otherwise        = CdChild $ ((drop 5).head) ls

getTree :: State (TreePos Full File) (Tree File)
getTree = get >>= (return.toTree)

constructDirTree :: Com -> State (TreePos Full File) (Tree File)
constructDirTree CdParent = modify (fromJust.parent) >> getTree
constructDirTree (Ls files) = modify (fromJust.parent.(insertFiles files).children) >> getTree
constructDirTree (CdChild dir) = do
    modify (fromJust.firstChild)
    findChild dir
    getTree

-- assumes treepos is in a subforest
findChild :: String -> State (TreePos Full File) ()
findChild dir = do
    l <- get >>= (return.label)
    case l of
        (Directory d) -> if (d == dir)
            then return ()
            else modify (fromJust.next) >> findChild dir
        _ -> modify (fromJust.next) >> findChild dir
 
insertFiles :: [File] -> TreePos Empty File -> TreePos Empty File    
insertFiles [] pos = pos
insertFiles (f : fs) pos = (insertFiles fs) . (nextSpace.(insert (Node f []))) $ pos  

size :: Tree File -> Integer
size (Node (Entry _ i) []) = i
size (Node (Directory _) ns) = sum $ size <$> ns

getSizes :: Tree File -> Tree Integer
getSizes (Node (Entry _ _) ns) = (Node 0 (fmap getSizes ns))
getSizes (Node f ns) = (Node (size (Node f ns)) (fmap getSizes ns))

sumSizes :: Tree Integer -> Integer
sumSizes = sum . filter (<= 100000) . flatten

largestSmallest :: Tree Integer -> Integer
largestSmallest t@(Node i _) = foldr min i $ filter (>= (30000000 - (70000000 - i))) $ flatten t

stuff lines = ((evalState (sequence (constructDirTree <$> toComs (tail lines)))) (fromTree (Node (Directory "root") [])))

toComs ls = fmap parseComGroup (group ls(startsWith '$'))
p1 = show.sumSizes.getSizes.last . stuff
p2 = show.largestSmallest.getSizes.last.stuff

day_seven :: [String] -> (String, String)
day_seven x = (p1 x, p2 x)
