module Parsing where

import Text.Parsec.String
import Text.Parsec
import Data.Either

integer :: Parser Integer
integer = read <$> many1 digit

listOfIntegers :: Parser [Integer]
listOfIntegers = many1 (spaces >> integer)

parseListOfInteger :: String -> [Integer]
parseListOfInteger = fromRight [] . runParser listOfIntegers () ""