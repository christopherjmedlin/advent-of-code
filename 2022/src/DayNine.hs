module DayNine
    (day_nine) where

import Data.Set
import Prelude hiding (tail, head)
import Control.Monad.State.Lazy

data RopeState = RopeState {tail :: (Int,Int), 
                            head :: (Int,Int),
                            visited :: Set (Int,Int)}

moveTail :: (Int,Int) -> State RopeState (Set (Int,Int))
moveTail (d1,d2) = do
    s <- get
    let (x,y) = tail s
    put $ RopeState (x+d1,y+d2) (head s) (visited s)
    return $ visited s
moveHead :: (Int,Int) -> State RopeState (Set (Int,Int))
moveHead (d1,d2) = do
    s <- get
    let (x,y) = tail s
    put $ RopeState (tail s) (x+d1,y+d2) (visited s)
    return $ visited s

parseMovement :: String -> State RopeState (Set (Int,Int))
parseMovement ['D',' ',c] = moveHead (0, -(read [c]))
parseMovement ['U',' ',c] = moveHead (0, read [c])
parseMovement ['R',' ',c] = moveHead (read [c], 0)
parseMovement ['L',' ',c] = moveHead (-(read [c]), 0)

follow :: State RopeState (Set (Int,Int))
follow = do
    s <- get
    let (hx, hy) = head s
    let (tx, ty) = tail s
    let diffx = tx-hx
    let diffy = ty-hy
    moveTail (signum diffx, signum diffy)

updateVisited :: State RopeState (Set (Int,Int))
updateVisited = do
    s <- get
    let newVisited = insert (tail s) (visited s)
    put $ RopeState (tail s) (head s) newVisited
    return newVisited

main :: [String] -> State RopeState (Set (Int,Int))
main [] = updateVisited
main (l:ls) = (parseMovement l) >> follow >> updateVisited >> main ls

run :: [String] -> Set (Int, Int)
run ls = evalState (main ls) (RopeState (0,0) (0,0) (fromList [(0,0)]))

p1 = size.run
day_nine lines = ("", "")
