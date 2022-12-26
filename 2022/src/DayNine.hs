module DayNine
    (day_nine) where

import Data.Set
import Prelude hiding (tail, head, Right, Left)
import Control.Monad.State.Lazy

data RopeState = RopeState {tail :: (Int,Int), 
                            head :: (Int,Int),
                            visited :: Set (Int,Int)} deriving Show
data Direction = Left | Right | Up | Down

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

parseMovement :: String -> State RopeState (Set (Int,Int))
parseMovement ('D' : (' ' : xs)) = makeMovement Down (read xs)
parseMovement ('U' : (' ' : xs)) = makeMovement Up (read xs)
parseMovement ('L' : (' ' : xs)) = makeMovement Left (read xs)
parseMovement ('R' : (' ' : xs)) = makeMovement Right (read xs)

follow :: State RopeState (Set (Int,Int))
follow = do
    s <- get
    let (hx, hy) = head s
    let (tx, ty) = tail s
    let diffx = hx-tx
    let diffy = hy-ty
    if (abs diffx <= 1) && (abs diffy <= 1)
        then return $ visited s
        else moveTail (signum diffx, signum diffy)

updateVisited :: State RopeState (Set (Int,Int))
updateVisited = do
    s <- get
    let newVisited = insert (tail s) (visited s)
    put $ RopeState (tail s) (head s) newVisited
    return newVisited

main :: [String] -> State RopeState (Set (Int,Int))
main strs = last <$> traverse parseMovement strs

run :: [String] -> Set (Int, Int)
run ls = evalState (main ls) (RopeState (0,0) (0,0) (fromList [(0,0)]))

p1 = show.size.run

day_nine lines = (p1 lines, "")
