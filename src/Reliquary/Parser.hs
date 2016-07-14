module Reliquary.Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim

import Control.Applicative ((<$>), (*>), (<*), (<*>))

import Reliquary.AST

parseProgram :: Parser [(String, Term)]
parseProgram = many (parseDefinition <* spaces)

parseDefinition :: Parser (String, Term)
parseDefinition =
        char '@' *>
        (extractWord <$> (spaces *> parseWord <* spaces)) <*>
        (spaces *> parseCompose <* spaces)
        <* char ';' where
            extractWord (Word w) t = (w, t)
            extractWord _ _ = undefined

parseCompose :: Parser Term
parseCompose = buildCompose . reverse <$> many (spaces *> parseExpr <* spaces) where
    buildCompose (x:y:xs) = Compose x $ buildCompose $ y:xs
    buildCompose (x:xs) = x
    buildCompose [] = undefined

parseExpr :: Parser Term
parseExpr = parseWord <|>
            parseLiteral <|>
            parseBlock

parseWord :: Parser Term
parseWord = Word <$>
        ((:) <$> (letter <|> special) <*> many (alphaNum <|> special)) where
            special = oneOf "~`!$%^&*_-+=|\\<>?/"

parseLiteral :: Parser Term
parseLiteral = Literal . read <$> many1 digit

parseBlock :: Parser Term
parseBlock = Block <$> (char '[' *> spaces *>
                              parseExpr
                        <* spaces <* char ']')
