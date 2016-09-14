module Reliquary.Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim

import Control.Applicative ((<$>), (*>), (<*), (<*>))

import Reliquary.AST

parseTerm :: Parser Term
parseTerm = parseDefinition
        <|> parseWord
        <|> parseLiteral
        <|> parseBlock
        <|> parseListType

parseDefinition :: Parser Term
parseDefinition =
        char '@' *>
        (extractWord <$> (spaces *> parseWord <* spaces))
        <*> (char ':' *> spaces *> parseTerm <* spaces)
        <*> (char '=' *> spaces *> parseTerm <* spaces)
        <*> (char ';' *> spaces *> parseTerm <* spaces) where
            extractWord (Word w) = Definition w
            extractWord _ = undefined

parseWord :: Parser Term
parseWord = Word <$>
        ((:) <$> (letter <|> special) <*> many (alphaNum <|> special)) where
            special = oneOf "~`!$%^&*_-+=|\\<>?/"

parseLiteral :: Parser Term
parseLiteral = Literal . read <$> many1 digit

parseBlock :: Parser Term
parseBlock = Block <$> (char '{' *> many (spaces *> parseTerm <* spaces) <* char '}')

parseListType :: Parser Term
parseListType = ListType <$> (char '[' *> many (spaces *> parseTerm <* spaces) <* char ']')
