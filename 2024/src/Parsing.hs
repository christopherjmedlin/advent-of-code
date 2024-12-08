module Parsing where

import Text.Parsec.String
import Text.Parsec
import Data.Either

integer :: Parser Integer
integer = read <$> many1 digit

listOfIntegers :: Parser [Integer]
listOfIntegers = many1 (spaces >> integer)

commaSeparatedIntegers :: Parser [Integer]
commaSeparatedIntegers = do
    i <- integer
    c <- try $ char ',' <|> return '.'
    if c == '.'
        then return [i]
        else
            do
                is <- commaSeparatedIntegers 
                return (i : is)

parseListOfInteger :: String -> [Integer]
parseListOfInteger = fromRight [] . runParser listOfIntegers () ""