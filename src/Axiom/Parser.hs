{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Axiom.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Control.Applicative ((<|>), optional)
import Data.Map (Map)
import Text.Megaparsec (some, many, try, manyTill, getSourcePos, getOffset, SourcePos (sourceName), ParsecT, between)
import Text.Megaparsec.Char (space1, char, string, alphaNumChar, letterChar, digitChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Axiom.Ast
import Control.Monad.Reader
import Control.Monad.Combinators (sepBy1)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, Prefix), makeExprParser)

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
symbolSpan t = spanOf <$> lexeme (locate $ string t)

integer :: Parser Int
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

name :: Parser Text
name = lexeme $ fmap T.pack (some letterChar)

number :: Parser Text
number = lexeme $ T.pack <$> (some digitChar)

-- locate wraps its argument, so put it inside the lexeme
-- to keep trailing whitespace out of the span
locate :: Parser a -> Parser (Spanned a)
locate p = do
    srcName <- sourceName <$> getSourcePos
    idMap <- lift $ asks sourceIds
    s <- getOffset
    val <- p
    e <- getOffset
    pure $ Spanned ( Span (fromJust $ Map.lookup srcName idMap) s e) val

-- Type keeps its span in its own field, so unwrap the Spanned here
locateType :: Parser TypeKind -> Parser Type
locateType p = lexeme $ locate p

-- unlexed, so callers can take a span that stops before trailing whitespace
identifier' :: Parser Text
identifier' = do
    c <- letterChar
    rest <- many alphaNumChar
    pure $ T.pack $ c : rest

identifier :: Parser Text
identifier = lexeme identifier'

typedVar :: Parser (Text, Type)
typedVar = do
    ident <- identifier <* symbol ":"
    t <- axiomType
    pure (ident, t)

enumType :: Parser Type
enumType = locateType $ TEnum <$> identifier'

-- type Example = Point(f32,f32) | Circle(f32)
paramType :: Parser Type
paramType = do
    enum <- lexeme $ locate identifier'
    args <- symbol "(" *> axiomType `sepBy1` symbol ","
    close <- symbolSpan ")"
    pure $ Spanned (spanOf enum <> close) (TParam (spanVal enum) args)

structType :: Parser Type
structType = do
    open <- symbolSpan "{"
    vars <- typedVar `sepBy1` symbol ","
    close <- symbolSpan "}"
    pure $ Spanned (open <> close) (TStruct vars)

-- a single variant is the type itself, not a one-element sum
sumType :: Parser Type
sumType = do
    types <- atomicType `sepBy1` symbol "|"
    pure $ case types of
        [t] -> t
        ts  -> Spanned (foldr1 (<>) (map spanOf ts)) (TSum ts) 

atomicType :: Parser Type
atomicType = try paramType
    <|> enumType
    <|> structType

axiomType :: Parser Type
axiomType = sumType

binary :: Text -> BinOp -> Parser (Expr -> Expr -> Expr)
binary s op = do
    symbol s
    pure $ \lhs rhs ->
        Spanned {
            spanVal = EBinary op lhs rhs,
            spanOf = spanOf lhs <> spanOf rhs
        }

unary :: Text -> UnOp -> Parser (Expr -> Expr)
unary s op = do
    opSpan <- symbolSpan s
    pure $ \e ->
        Spanned {
            spanVal = EUnary op e,
            spanOf = opSpan <> spanOf e
        }

-- tightest first: the earlier the group, the tighter it binds.
-- longer operators come before their prefixes (">=" before ">"),
-- since symbol commits as soon as it matches
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [
        [
            Prefix $ unary "!" Neg
        ],
        [
            InfixR $ binary "^" Pow
        ],
        [
            InfixL (binary "*" Mul),
            InfixL (binary "/" Div)
        ],
        [
            InfixL (binary "+" Plus),
            InfixL (binary "-" Minus)
        ],
        [
            InfixL (binary ">=" Bte),
            InfixL (binary ">" Bt),
            InfixL (binary "<=" Lte),
            InfixL (binary "<" Lt),
            InfixL (binary "==" Eq),
            InfixL (binary "!=" Ne)
        ],
        [
            InfixL (binary "&&" LogicAnd)
        ],
        [
            InfixL (binary "||" LogicOr)
        ]
    ]


grouped :: Parser Expr
grouped = do
    open <- symbolSpan "("
    e <- expr
    close <- symbolSpan ")"
    pure $ Spanned {
        spanVal = EGrouped e,
        spanOf = open <> close
    }

-- unlexed, like identifier', so the atom's span stops at its last character
atom :: Parser Atom
atom = LFloat <$> try L.float
    <|> LInt <$> L.decimal
    <|> LStr <$> stringLit
    <|> LVar <$> identifier'

stringLit :: Parser Text
stringLit = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- probably a bad name
atomicExpr :: Parser Expr
atomicExpr = grouped
    <|> lexeme (locate $ EAtom <$> atom)

ifExpr :: Parser Expr
ifExpr = do
    s <- symbolSpan "if"
    cond <- expr
    action <- expr
    elBlock <- optional expr
    pure $ Spanned {
        spanVal = EIf cond action elBlock,
        spanOf = case elBlock of
            Just e ->  s <> spanOf e
            Nothing -> s <> spanOf action
    }

blockExpr :: Parser Expr
blockExpr = do 
    se <- locate $ between (symbol "{") (symbol "}") (many expr)
    pure $ EBlock <$> se

callExpr :: Parser Expr
callExpr = do
    ident <- lexeme $ locate identifier
    params <- locate $ between (symbol "(") (symbol ")") $ expr `sepBy1` (symbol ",")
    pure $ Spanned {
        spanOf = (spanOf ident) <> (spanOf params),
        spanVal = EFuncCall (spanVal ident) (spanVal params)
    }


expr :: Parser Expr
expr = makeExprParser atomicExpr operatorTable
