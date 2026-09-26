{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Axiom.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Control.Applicative ((<|>), optional)
import Data.Map (Map)
import Text.Megaparsec (some, many, try, manyTill, getSourcePos, getOffset, SourcePos (sourceName), ParsecT, between, sepBy, choice, chunk, MonadParsec (notFollowedBy), satisfy)
import Text.Megaparsec.Char (space1, char, string, alphaNumChar, letterChar, digitChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Axiom.Ast
import Control.Monad.Reader
import Control.Monad.Combinators (sepBy1)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, Prefix), makeExprParser)
import Data.Function (on)
import Data.Char
import qualified Text.Megaparsec.Char (char )

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

symbol' :: Text -> Parser Text
symbol' = chunk

-- can't rely on nodes to capture start and end by themselves
-- some symbol dont appear in the node body (e.g. '{' ',')
-- this function assumes that responsibility
symbolSpan :: Text -> Parser Span
symbolSpan t = spanOf <$> lexeme (locate $ string t)

integer :: Parser Int
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float


keyword :: Text -> Parser Text
keyword kw = lexeme $ try (string kw <* notFollowedBy (satisfy (not . isSpace)))

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
    s <- locate $ keyword "if"
    cond <- expr
    action <- keyword "then" *> expr
    elBlock <- optional $ keyword "else" *> (ifExpr <|> expr)
    pure $ Spanned {
        spanVal = EIf cond action elBlock,
        spanOf = case elBlock of
            Just e ->  spanOf s <> spanOf e
            Nothing -> spanOf s <> spanOf action
    }

blockExpr :: Parser Expr
blockExpr = do 
    -- use lexeme to skip space since symbol' doesn't 
    se <- lexeme . locate $ between (symbol "{") (symbol' "}") (many expr)
    pure $ EBlock <$> se

callExpr :: Parser Expr
callExpr = do
    ident <- lexeme $ locate identifier
    -- use lexeme to skip space since symbol' doesn't 
    params <- lexeme . locate $ between (symbol "(") (symbol' ")") $ expr `sepBy1` (symbol ",")
    pure $ Spanned {
        spanOf = (spanOf ident) <> (spanOf params),
        spanVal = EFuncCall (spanVal ident) (spanVal params)
    }

listIndex :: Parser Expr
listIndex = do
    ident <- lexeme $ locate identifier
    e <- lexeme . locate $ between (symbol "[") (symbol' "]") expr
    pure $ Spanned {
        spanOf = (spanOf ident) <> (spanOf e),
        spanVal = EListIndex (spanVal ident) (spanVal e)
    }

fieldAccess :: Parser Expr
fieldAccess = do
    e <- expr
    ident <- (char '.') *> (lexeme $ locate identifier')
    return Spanned {
        spanOf = (spanOf e) <> (spanOf ident),
        spanVal = EFieldAccess e (spanVal ident)
    }

range :: Parser Expr
range = do
    e1 <- expr
    symbol ".."
    e2 <- expr
    pure $ Spanned {
        spanOf = spanOf e1 <> spanOf e2,
        spanVal = ERange e1 e2
    }

while :: Parser Expr
while = do
    kw <- locate $ keyword "while"
    e1 <- expr
    lexeme $ keyword "do"
    e2 <- expr
    pure $ Spanned {
        spanOf = spanOf kw <> spanOf e2,
        spanVal = EWhile e1 e2
    }

for :: Parser Expr
for = undefined

match :: Parser Expr
match = do
    kw <- locate $ keyword "match"
    e <- expr
    matchCases <- locate $ between (symbol "{") (symbol' "}") $ many parseCase
    pure $ Spanned {
        spanOf = spanOf kw <> spanOf matchCases,
        spanVal = EMatch e $ spanVal matchCases
    }
    where
        parseCase :: Parser (Pattern, Expr)
        parseCase = do
            pat <- parsePattern
            symbol "->"
            e <- expr
            pure (pat, e)

expr :: Parser Expr
expr = makeExprParser atomicExpr operatorTable


listPat :: Parser Pattern
listPat = do
    open <- symbolSpan "["
    pat <- parsePattern `sepBy` symbol ","
    close <- symbolSpan "]"
    return $ Spanned {
        spanOf = open <> close,
        spanVal = PList pat
    }


tuplePat :: Parser Pattern
tuplePat = do
    open <- symbolSpan "("
    pat <- parsePattern `sepBy` symbol ","
    close <- symbolSpan ")"
    return $ Spanned {
        spanOf = open <> close,
        spanVal = PTuple pat
    }


litPat :: Parser Pattern
litPat = (lexeme . locate) $  PLit <$> atom

variantPat :: Parser Pattern
variantPat = do
    ident <- lexeme $ locate identifier'
    pats <- tuplePat
    let (PTuple ps) = spanVal pats
    pure $ Spanned {
        spanOf = spanOf ident <> spanOf pats,
        spanVal = PVariant (spanVal ident)  ps
    }

-- TODO: handle ordered destructuring
structPat :: Parser Pattern
structPat = do
    open <- symbolSpan "{"
    pats <- ((,) <$> (identifier <* symbol ":") <*> parsePattern) `sepBy` (symbol ",")
    close <- symbolSpan "}"
    pure $ Spanned {
        spanOf = open <> close,
        spanVal = PStruct pats
    }

wildcardPat :: Parser Pattern
wildcardPat = do 
    s <- symbolSpan "_"
    pure $ Spanned {
        spanOf = s,
        spanVal = PWildCard
    }

atomicPat :: Parser Pattern
atomicPat = litPat <|> wildcardPat

complexPat :: Parser Pattern
complexPat = choice [
    listPat,
    tuplePat,
    structPat,
    try variantPat,
    atomicPat -- this should be last as its the simplest
    ]

orPat :: Parser Pattern
orPat = do 
    pats <- complexPat `sepBy1` (symbol "|")
    let f = head pats
    let l = last pats
    pure $ Spanned {
        spanOf = spanOf f <> spanOf l,
        spanVal = POr pats
    }

parsePattern :: Parser Pattern
parsePattern = do
    pats <- orPat
    pure $ case spanVal pats of
        POr [p] -> p
        _ -> pats