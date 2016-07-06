module Reliquary.Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim

import Control.Applicative ((<$>), (*>), (<*), (<*>))

import Reliquary.AST

parseProgram :: Parser [Definition]
parseProgram = many (parseDefinition <* spaces)

parseDefinition :: Parser Definition
parseDefinition =
        char '@' *>
        (extractWord <$> (spaces *> parseWord <* spaces)) <*>
        (Quasi <$> (spaces *> many (parseExpr <* spaces) <* spaces))
        <* char ';' where
            extractWord (Word w) = Definition w
            extractWord _ = undefined

parseExpr :: Parser Term
parseExpr = parseWord <|>
            parseLiteral <|>
            char '[' *> parseBlock <* char ']' <|>
            char '{' *> parseQuasi <* char '}'

parseWord :: Parser Term
parseWord = Word <$>
        ((:) <$> (letter <|> special) <*> many (alphaNum <|> special)) where
            special = oneOf "~`!$%^&*_-+=|\\<>?/"

parseLiteral :: Parser Term
parseLiteral = Literal . read <$> many1 digit

parseBlock :: Parser Term
parseBlock = Block <$> many (parseExpr <* spaces) 

parseQuasi :: Parser Term
parseQuasi = extractBlock <$> parseBlock where
    extractBlock (Block exprs) = Quasi exprs
    extractBlock _ = undefined
