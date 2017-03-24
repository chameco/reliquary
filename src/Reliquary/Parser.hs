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
parseRepl s = case parse (Block <$> many (spaces *> parseTerm <* spaces)) "<stdin>" s of Left e -> throwError $ SyntaxError e
                                                                                         Right t -> return t

parseDict :: Parser SourceDictionary
parseDict = many (spaces *> parseEntry <* spaces)

parseEntry :: Parser SourceEntry
parseEntry =
        char '@' *> (extractWord <$> (spaces *> parseWord <* spaces))
        <*> (char '=' *> spaces *> parseTerm <* spaces) where
            extractWord (Word w) = SourceEntry w
            extractWord _ = undefined

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
parseBlock = Block <$> (char '{' *> many (spaces *> parseTerm <* spaces) <* char '}')
