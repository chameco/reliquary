module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim

import Control.Applicative ((<$>), (*>), (<*), (<*>))

import Model

parseProgram :: Parser Expr
parseProgram = extractBlock <$> parseBlock  where
    extractBlock (Block exprs) = Program exprs
    extractBlock _ = undefined

parseExpr :: Parser Expr
parseExpr = parseWord <|>
            parseLiteral <|>
            char '[' *> parseBlock <* char ']' <|>
            char '@' *> parseDefinition <* char ';'

parseWord :: Parser Expr
parseWord = Word <$>
        ((:) <$> (letter <|> special) <*> many (alphaNum <|> special)) where
            special = oneOf "~`!$%^&*_-+=|\\<>?/"

parseLiteral :: Parser Expr
parseLiteral = Literal . read <$> many1 digit

parseBlock :: Parser Expr
parseBlock = Block <$> many (parseExpr <* spaces) 

parseDefinition :: Parser Expr
parseDefinition =
        (extractWord <$> (spaces *> parseWord <* spaces)) <*>
        (spaces *> parseProgram <* spaces) where
            extractWord (Word w) = Definition w
            extractWord _ = undefined
