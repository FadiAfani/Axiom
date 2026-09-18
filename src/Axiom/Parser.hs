{-# LANGUAGE OverloadedStrings #-}

module Axiom.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Control.Applicative ((<|>))
import Data.Map (Map)
import Text.Megaparsec (Parsec, getInput, some, many, getSourcePos, getOffset, SourcePos (sourceName), ParsecT)
import Text.Megaparsec.Char (space1, char, alphaNumChar, letterChar, digitChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Axiom.Ast (AxiomType (TVar, EnumT), VariantType (VariantType), Span (Span), Identifier (idSpan, Identifier))
import Control.Monad.Reader
import Control.Monad.Combinators (between, sepBy1)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data ParseEnv = ParseEnv {
    sourceIds :: Map String Int
}

type Parser = ParsecT Void Text (Reader ParseEnv)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

name :: Parser Text
name = lexeme $ fmap T.pack (some letterChar)

number :: Parser Text
number = lexeme $ T.pack <$> (some digitChar)

locate :: Parser (Span -> a) -> Parser a
locate p = do
    srcName <- sourceName <$> getSourcePos
    idMap <- lift $ asks sourceIds
    s <- getOffset
    f <- p
    e <- getOffset
    pure $ f $ Span (fromJust $ Map.lookup srcName idMap) s e

identifier :: Parser Identifier
identifier = do
    c <- letterChar
    rest <- many (number <|> name)
    lexeme $ locate $ pure $ Identifier (T.cons c $ T.concat rest)

tupleType :: Parser VariantType
tupleType = do
    enum <- identifier
    args <- between
        (symbol "(")
        (symbol ")")
        (axiomType `sepBy1` symbol ",")
    locate $ pure $ VariantType enum args


axiomType :: Parser AxiomType
axiomType = (EnumT <$> identifier) <|> (TVar <$> tupleType)
