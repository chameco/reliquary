module Reliquary.Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Control.Monad.Except

import Reliquary.Utils.Error

import Reliquary.AST
import Reliquary.Dictionary

parseRepl :: String -> Either GenError Term
parseRepl s = case parse parseTerm "<stdin>" s of Left e -> throwError $ SyntaxError e
                                                  Right t -> return t

parseTerm :: Parser Term
parseTerm = parseWord
        <|> parseLiteral
        <|> parseBlock

parseWord :: Parser Term
parseWord = Word <$>
        ((:) <$> (letter <|> special) <*> many (alphaNum <|> special)) where
            special = oneOf "*()~`!$%^&_-+=|\\<>?/"

parseLiteral :: Parser Term
parseLiteral = Literal . read <$> many1 digit

parseBlock :: Parser Term
parseBlock = Block <$> (char '[' *> many (spaces *> parseTerm <* spaces) <* char ']')
