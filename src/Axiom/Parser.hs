{-# LANGUAGE OverloadedStrings #-}

module Axiom.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Control.Applicative ((<|>))
import Data.Map (Map)
import Text.Megaparsec (Parsec, getInput, some, many, getSourcePos, getOffset, SourcePos (sourceName), ParsecT)
import Text.Megaparsec.Char (space1, string, alphaNumChar, letterChar, digitChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Axiom.Ast (spanOf, Expr, Span (Span), Identifier (Identifier), Type (Type), TypeKind (TEnum, TParam, TStruct, TRefine, TSum))
import Control.Monad.Reader
import Control.Monad.Combinators (sepBy1)
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

-- can't rely on nodes to capture start and end by themselves
-- some symbol dont appear in the node body (e.g. '{' ',')
-- this function assumes that responsibility 
symbolSpan :: Text -> Parser Span
symbolSpan t = lexeme $ locate (id <$ string t)

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
identifier = lexeme $ locate $ do
    c <- letterChar
    rest <- many alphaNumChar
    pure $ Identifier (T.pack $ c : rest)

typedVar :: Parser (Identifier, Type)
typedVar = do
    ident <- identifier <* symbol ":"
    t <- axiomType
    pure (ident, t)

enumType :: Parser Type
enumType = do
    ident <- identifier
    pure $ Type (TEnum ident) (spanOf ident)

-- type Example = Point(f32,f32) | Circle(f32)
paramType :: Parser Type
paramType = do
    enum <- identifier
    args <- symbol "(" *> axiomType `sepBy1` symbol ","
    close <- symbolSpan ")"
    pure $ Type (TParam enum args) (spanOf enum <> close)

structType :: Parser Type
structType = do
    open <- symbolSpan "{"
    vars <- typedVar `sepBy1` symbol ","
    close <- symbolSpan "}"
    pure $ Type (TStruct vars) (open <> close)

refType :: Parser Type
refType = do
    open <- symbolSpan "{"
    (var, t) <- typedVar
    e <- expr
    close <- symbolSpan "}"
    pure $ Type (TRefine var t e) (open <> close)

expr :: Parser Expr
expr = undefined

-- a single variant is the type itself, not a one-element sum
sumType :: Parser Type
sumType = do
    types <- atomicType `sepBy1` symbol "|"
    pure $ case types of
        [t] -> t
        ts  -> Type (TSum ts) (foldr1 (<>) (map spanOf ts))

atomicType :: Parser Type
atomicType = enumType
    <|> structType
    <|> refType

axiomType :: Parser Type
axiomType = sumType
