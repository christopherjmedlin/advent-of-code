module DayNine
    (day_nine) where

import Data.Set
import Prelude hiding (tail, head, Right, Left)
import Control.Monad.State.Lazy

data RopeState = RopeState {tail :: (Int,Int), 
                            head :: (Int,Int),
                            visited :: Set (Int,Int)} deriving Show
data RopeState' = RopeState' {knots :: [(Int, Int)],
                             visited' :: Set (Int,Int)} deriving Show
data Direction = Left | Right | Up | Down

head' :: RopeState' -> (Int, Int)
head' rs = f (knots rs)
    where f (x : xs) = x

tail' rs = f (knots rs)
    where f (x : xs) = xs

moveTail :: (Int,Int) -> State RopeState (Set (Int,Int))
moveTail (d1,d2) = do
    s <- get
    let (x,y) = tail s
    put $ RopeState (x+d1,y+d2) (head s) (visited s)
    return $ visited s

makeMovement :: Direction -> Integer -> State RopeState (Set (Int,Int))
makeMovement dir 0 = updateVisited
makeMovement dir i = do 
    nudge dir
    follow
    updateVisited
    makeMovement dir (i - 1)

nudge :: Direction -> State RopeState (Set (Int,Int))
nudge dir = do
    s <- get
    let h = head s
    put $ RopeState (tail s) (newPos dir (head s)) (visited s)
    return (visited s)
    where
        newPos Left (x,y) = (x-1, y)
        newPos Right (x,y) = (x+1, y)
        newPos Down (x,y) = (x, y-1)
        newPos Up (x,y) = (x, y+1)

makeMovement' :: Direction -> Integer -> State RopeState' (Set (Int,Int))
makeMovement' dir 0 = updateVisited'
makeMovement' dir i = do 
    nudge' dir
    follow'
    updateVisited'
    makeMovement' dir (i - 1)

nudge' :: Direction -> State RopeState' (Set (Int,Int))
nudge' dir = do
    s <- get
    let new = newPos dir (head' s)
    put $ RopeState' (new : (tail' s)) (visited' s)
    return (visited' s)
    where
        newPos Left (x,y) = (x-1, y)
        newPos Right (x,y) = (x+1, y)
        newPos Down (x,y) = (x, y-1)
        newPos Up (x,y) = (x, y+1)

parseMovement :: String -> State RopeState (Set (Int,Int))
parseMovement ('D' : (' ' : xs)) = makeMovement Down (read xs)
parseMovement ('U' : (' ' : xs)) = makeMovement Up (read xs)
parseMovement ('L' : (' ' : xs)) = makeMovement Left (read xs)
parseMovement ('R' : (' ' : xs)) = makeMovement Right (read xs)

parseMovement' :: String -> State RopeState' (Set (Int,Int))
parseMovement' ('D' : (' ' : xs)) = makeMovement' Down (read xs)
parseMovement' ('U' : (' ' : xs)) = makeMovement' Up (read xs)
parseMovement' ('L' : (' ' : xs)) = makeMovement' Left (read xs)
parseMovement' ('R' : (' ' : xs)) = makeMovement' Right (read xs)

follow :: State RopeState (Set (Int,Int))
follow = do
    s <- get
    let (hx, hy) = head s
    let (tx, ty) = tail s
    let diffx = hx-tx
    let diffy = hy-ty
    moveTail $ moveTowards (head s) (tail s)

follow' :: State RopeState' (Set (Int,Int))
follow' = do
    s <- get
    let new = followLongTail (knots s)
    put $ RopeState' new (visited' s)
    return $ visited' s

moveTowards :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTowards (tx, ty) (mx, my)
    | (abs diffx <= 1) && (abs diffy <= 1) = (0,0)
    | otherwise = (signum diffx, signum diffy)
    where
        diffx = tx - mx
        diffy = ty - my

followLongTail :: [(Int, Int)] -> [(Int, Int)]
followLongTail (x : xs) = x : go x xs
    where
        go prev []       = []
        go prev (x : xs) = n : go n xs
            where (dx, dy) = (moveTowards prev x)
                  (cx, cy) = x
                  n        = (cx + dx, cy + dy)

updateVisited :: State RopeState (Set (Int,Int))
updateVisited = do
    s <- get
    let newVisited = insert (tail s) (visited s)
    put $ RopeState (tail s) (head s) newVisited
    return newVisited

updateVisited' :: State RopeState' (Set (Int,Int))
updateVisited' = do
    s <- get
    let newVisited = insert (last (knots s)) (visited' s)
    put $ RopeState' (knots s) newVisited
    return $ visited' s

main :: [String] -> State RopeState (Set (Int,Int))
main strs = last <$> traverse parseMovement strs

main' :: [String] -> State RopeState' (Set (Int, Int))
main' strs = last <$> traverse parseMovement' strs

run :: [String] -> Set (Int, Int)
run ls = evalState (main ls) (RopeState (0,0) (0,0) (fromList [(0,0)]))

run' :: [String] -> Set (Int, Int)
run' ls = evalState (main' ls) (RopeState' (replicate 10 (0,0)) (fromList [(0,0)]))

p1 = show.size.run
p2 = show.size.run'

day_nine lines = (p1 lines, p2 lines)
