module DaySix where

import Util (matrix, inBounds)
import Control.Monad.State
import Data.Set ( Set, insert, empty, member )
import Data.Array ( (!), indices, Array, bounds, (//) )
import Data.Bifunctor

data Position = Obstruction | Empty deriving (Eq, Show)
data Direction = GoLeft | GoRight | Up | Down deriving (Eq, Ord)

type Bounds = ((Int, Int), (Int, Int))

type GuardState = ((Int, Int), Direction, Set (Direction, (Int, Int)))


visitBounded :: ((Int, Int) -> Position) -> Bounds -> State ((Int, Int), Direction) (Set (Int, Int))
visitBounded f b = do
    (pos, dir) <- get
    if inBounds (move pos dir) b then visit f b else return (insert pos empty)

visit :: ((Int, Int) -> Position) -> Bounds -> State ((Int, Int), Direction) (Set (Int, Int))
visit f bounds = do
    (pos, dir) <- get
    let newPos = move pos dir
    let newPosState = f newPos

    if newPosState == Obstruction
        then
            put (pos, rotate dir)
        else
            put (newPos, dir)

    insert pos <$> visitBounded f bounds

loopBounded :: ((Int, Int) -> Position) -> Bounds -> State GuardState Bool
loopBounded f b = do
    (pos, dir, visited) <- get
    if inBounds (move pos dir) b then loop f b else return False

loop :: ((Int, Int) -> Position) -> Bounds -> State GuardState Bool
loop f bounds = do
    (pos, dir, visited) <- get
    let newPos = move pos dir
    let newPosState = f newPos

    if newPosState == Obstruction
        then
            put (pos, rotate dir, insert (rotate dir, pos) visited)
        else
            put (newPos, dir, insert (dir, newPos) visited)

    if not (member (dir, newPos) visited)
        then loopBounded f bounds
        else return True

rotate :: Direction -> Direction
rotate GoRight = Down
rotate Down = GoLeft
rotate GoLeft = Up
rotate Up = GoRight

move :: (Int, Int) -> Direction -> (Int, Int)
move (i1, i2) GoRight = (i1, i2+1)
move (i1, i2) GoLeft  = (i1, i2-1)
move (i1, i2) Up    = (i1-1, i2)
move (i1, i2) Down  = (i1+1, i2)

getGuardPosition :: Array (Int, Int) Char -> (Int, Int)
getGuardPosition arr = if null fil then (-1, -1) else head fil
    where fil = filter (\x -> arr ! x == '^') (indices arr)

parsePos :: Char -> Position
parsePos '#' = Obstruction
parsePos _   = Empty

substitutions :: Array (Int, Int) Char -> Char -> [Array (Int, Int) Char]
substitutions arr x = go
    where go = fmap (\y -> arr // [(y, x)]) (indices arr)
          sub y = if arr ! y == '^' then arr // [] else arr // [(y, x)]

p1 :: [String] -> String
p1 ls = show $ length $ evalState (visitBounded get matBounds) (getGuardPosition mat, Up)
    where
        mat = matrix ls
        posMat = parsePos <$> mat
        get = (posMat !)
        matBounds = bounds mat

isLoop :: Array (Int, Int) Char  -> Bool
isLoop mat = evalState (loopBounded get matBounds) (guardPos, Up, insert (Up, guardPos) empty)
    where
        guardPos = getGuardPosition mat
        posMat = parsePos <$> mat
        get = (posMat !)
        matBounds = bounds mat

p2 :: [String] -> String
p2 ls = show $ length $ filter isLoop (substitutions mat '#')
    where
        mat = matrix ls

daySix :: [String] -> (String, String)
daySix ls = (p1 ls, p2 ls)