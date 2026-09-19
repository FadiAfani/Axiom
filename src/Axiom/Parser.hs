{-# LANGUAGE OverloadedStrings #-}

module Axiom.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Control.Applicative ((<|>))
import Data.Map (Map)
import Text.Megaparsec (Parsec, getInput, some, many, getSourcePos, getOffset, SourcePos (sourceName), ParsecT)
import Text.Megaparsec.Char (space1, char, alphaNumChar, letterChar, digitChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Axiom.Ast (Expr, AtomicType (TEnum, TStruct, TRef), Span (Span), Identifier (idSpan, Identifier), StructType (StructType), RefinementType (RefinementType), ParamType (ParamType), SumType (SumType), AtomicType, AxiomType (TSum))
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

typedVar :: Parser (Identifier, AxiomType)
typedVar = do
    ident <- identifier <* symbol ":"
    t <- axiomType
    pure $ (ident, t)

-- type Example = Point(f32,f32) | Circle(f32)
paramType :: Parser ParamType
paramType = do
    enum <- identifier
    args <- between
        (symbol "(")
        (symbol ")")
        (axiomType `sepBy1` symbol ",")
    locate $ pure $ ParamType enum args

structType :: Parser StructType
structType = between (symbol "{") (symbol "}") $ do
    vars <- typedVar `sepBy1` symbol ","
    locate $ pure $ StructType vars

refType :: Parser RefinementType
refType = between (symbol "{") (symbol "}") $ do
    (var, t) <- typedVar
    e <- expr
    locate $ pure $ RefinementType var t e

expr :: Parser Expr
expr = undefined

sumType :: Parser SumType
sumType = locate $ SumType <$> (atomicType `sepBy1` symbol "|")

atomicType :: Parser AtomicType
atomicType = (TEnum <$> identifier)
    <|> (TStruct <$> structType)
    <|> (TRef <$> refType)

axiomType :: Parser AxiomType
axiomType = TSum <$> sumType
