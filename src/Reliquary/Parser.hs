module Reliquary.Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim

import Control.Applicative ((<$>), (*>), (<*), (<*>))

import Reliquary.AST
import Reliquary.Dictionary

parseProgram :: Parser Dictionary
parseProgram = many (parseDefinition <* spaces)

parseDefinition :: Parser Entry
parseDefinition =
        char '@' *>
        (extractWord <$> (spaces *> parseWord <* spaces))
        <*> (char ':' *> char ':' *> spaces *> parseTerm <* spaces)
        <*> (char '=' *> spaces *> parseTerm <* spaces <* char ';') where
            extractWord (Word w) = Entry w
            extractWord _ = undefined

parseTerm :: Parser Term
parseTerm = parseWord <|>
            parseLiteral <|>
            parseBlock

parseWord :: Parser Term
parseWord = Word <$>
        ((:) <$> (letter <|> special) <*> many (alphaNum <|> special)) where
            special = oneOf "~`!$%^&*_-+=|\\<>?/"

parseLiteral :: Parser Term
parseLiteral = Literal . read <$> many1 digit

parseBlock :: Parser Term
parseBlock = Block <$> (char '[' *> many (spaces *> parseTerm <* spaces) <* char ']')
