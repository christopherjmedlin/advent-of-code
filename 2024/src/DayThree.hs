module DayThree where

import Text.Parsec
    ( anyChar, char, string, (<|>), many, runParser, try )
import Text.Parsec.String ( Parser )
import Parsing (integer)
import Data.Monoid (Product(Product))
import Data.Either (fromRight)
import Data.Foldable (fold)
import Control.Monad.State

data Instruction = Do | Dont | Mul (Integer, Integer) deriving (Show)

mul :: Parser (Integer, Integer)
mul = do
    string "mul("
    i1 <- integer
    char ','
    i2 <- integer
    char ')'
    return (i1, i2)

instr :: Parser Instruction
instr = try parseDo <|> try parseDont <|> try parseMul

parseDo :: Parser Instruction
parseDo = string "do()" >> return Do

parseDont :: Parser Instruction
parseDont = string "don't()" >> return Dont

parseMul :: Parser Instruction
parseMul = Mul <$> mul

muls :: Parser [(Integer, Integer)]
muls = many loop
    where
        loop = try mul <|> try (anyChar >> loop)

instrs :: Parser [Instruction]
instrs = many loop
    where
        loop = try instr <|> try (anyChar >> loop)

addMuls :: [(Integer, Integer)] -> Integer
addMuls = sum . fmap (uncurry (*))

runInstruction :: Instruction -> State (Bool, Integer) Integer
runInstruction Do = do
    (b, i) <- get
    put (True, i)
    return i
runInstruction Dont = do
    (_, i) <- get
    put (False, i)
    return i
runInstruction (Mul (x, y)) = do
    (b, i) <- get
    if b
        then put (b, i + (x * y))  >> return (i + (x * y))
        else return i

runInstructions :: [Instruction] -> Integer
runInstructions instrs = last $ evalState (mapM runInstruction instrs) (True, 0)

p2 :: [String] -> String
p2 = show . runInstructions . fromRight [] . runParser instrs () "" . fold

p1Line :: String -> Integer
p1Line = addMuls . fromRight [] . runParser muls () ""

p1 :: [String] -> String
p1 = show . p1Line . fold

dayThree :: [String] -> (String, String)
dayThree ls = (p1 ls, p2 ls)