{-# LANGUAGE OverloadedStrings #-}

module Axiom.ParserTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Axiom.Parser (Parser, ParseEnv (ParseEnv, sourceIds), identifier, enumType, sumType, structType, paramType, expr, refType)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Monad.Reader (runReader)
import Text.Megaparsec (runParserT, MonadParsec (eof))
import qualified Data.Map as Map
import Axiom.Ast
    ( Span (Span)
    , Spanned (Spanned, spanOf)
    , Type (Type)
    , TypeKind (TEnum, TParam, TStruct, TSum, TRefine)
    , Expr
    , ExprKind (EAtom, EBinary, EUnary, EGrouped)
    , Atom (LVar, LStr, LInt, LFloat)
    , BinOp (..)
    , UnOp (Neg)
    )

testEnv :: ParseEnv
testEnv = ParseEnv {
    sourceIds = Map.fromList [("", 0)]
}

runTestParser :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runTestParser p input = runReader (runParserT (p <* eof) "" input) testEnv

parseFails :: Show a => Parser a -> Text -> Assertion
parseFails p input = case runTestParser p input of
    Left _ -> pure ()
    Right r -> assertFailure $ "expected parse failure, but got: " ++ show r

sp :: Int -> Int -> Span
sp = Span 0

enum :: Text -> Int -> Int -> Type
enum t s e = Type (TEnum t) (sp s e)

sumOf :: [Type] -> Int -> Int -> Type
sumOf ts s e = Type (TSum ts) (sp s e)

struct :: [(Text, Type)] -> Int -> Int -> Type
struct vars s e = Type (TStruct vars) (sp s e)

param :: Text -> [Type] -> Int -> Int -> Type
param i args s e = Type (TParam i args) (sp s e)

refine :: Text -> Type -> Expr -> Int -> Int -> Type
refine v t e s en = Type (TRefine v t e) (sp s en)

var :: Text -> Int -> Int -> Expr
var v s e = Spanned (sp s e) (EAtom (LVar v))

int :: Int -> Int -> Int -> Expr
int n s e = Spanned (sp s e) (EAtom (LInt n))

float :: Double -> Int -> Int -> Expr
float d s e = Spanned (sp s e) (EAtom (LFloat d))

str :: Text -> Int -> Int -> Expr
str t s e = Spanned (sp s e) (EAtom (LStr t))

-- structural helpers: the span is derived the same way the parser derives it,
-- so these tests are about shape only. spans are pinned down in exprSpanTests
bin :: BinOp -> Expr -> Expr -> Expr
bin op l r = Spanned (spanOf l <> spanOf r) (EBinary op l r)

exprSpan :: Text -> Either (ParseErrorBundle Text Void) Span
exprSpan input = spanOf <$> runTestParser expr input

tests :: TestTree
tests =
    testGroup "Parser"
    [ identifierTests
    , enumTypeTests
    , sumTypeTests
    , structTypeTests
    , paramTypeTests
    , atomTests
    , precedenceTests
    , associativityTests
    , groupedTests
    , unaryTests
    , exprSpanTests
    , exprWhitespaceTests
    , exprFailureTests
    , refTypeTests
    ]

identifierTests :: TestTree
identifierTests =
    testGroup "identifier"
    [ testCase "single letter" $
        runTestParser identifier "x" @?= Right "x"
    , testCase "letters and digits" $
        runTestParser identifier "abc123" @?= Right "abc123"
    , testCase "interleaved letters and digits" $
        runTestParser identifier "x1y" @?= Right "x1y"
    , testCase "consumes trailing whitespace" $
        runTestParser identifier "x  " @?= Right "x"
    , testCase "rejects whitespace inside the name" $
        parseFails identifier "ab cd"
    , testCase "rejects leading digit" $
        parseFails identifier "1x"
    , testCase "rejects empty input" $
        parseFails identifier ""
    ]

-- the name has no span of its own any more, so the enclosing type carries it
enumTypeTests :: TestTree
enumTypeTests =
    testGroup "enumType"
    [ testCase "span covers the name" $
        runTestParser enumType "int" @?= Right (enum "int" 0 3)
    , testCase "span excludes trailing whitespace (single letter)" $
        runTestParser enumType "x  " @?= Right (enum "x" 0 1)
    , testCase "span excludes trailing whitespace (multi letter)" $
        runTestParser enumType "int  " @?= Right (enum "int" 0 3)
    ]

sumTypeTests :: TestTree
sumTypeTests =
    testGroup "sumType"
    [ testCase "single variant is not wrapped in a sum" $
        runTestParser sumType "int" @?= Right (enum "int" 0 3)
    , testCase "multiple variants" $
        runTestParser sumType "A | B | C"
            @?= Right (sumOf [enum "A" 0 1, enum "B" 4 5, enum "C" 8 9] 0 9)
    , testCase "variants without spaces" $
        runTestParser sumType "Red|Green"
            @?= Right (sumOf [enum "Red" 0 3, enum "Green" 4 9] 0 9)
    , testCase "comments between variants" $
        runTestParser sumType "A // c\n| B"
            @?= Right (sumOf [enum "A" 0 1, enum "B" 9 10] 0 10)
    , testCase "struct variant" $
        runTestParser sumType "{x: int} | B"
            @?= Right (sumOf
                [ struct [("x", enum "int" 4 7)] 0 8
                , enum "B" 11 12
                ] 0 12)
    , testCase "rejects trailing bar" $
        parseFails sumType "A |"
    , testCase "rejects empty variant" $
        parseFails sumType "A | | B"
    ]

structTypeTests :: TestTree
structTypeTests =
    testGroup "structType"
    [ testCase "single field" $
        runTestParser structType "{x: int}"
            @?= Right (struct [("x", enum "int" 4 7)] 0 8)
    , testCase "multiple fields" $
        runTestParser structType "{x: int, y: int}"
            @?= Right (struct
                [ ("x", enum "int" 4 7)
                , ("y", enum "int" 12 15)
                ] 0 16)
    , testCase "whitespace inside braces" $
        runTestParser structType "{ x : int }"
            @?= Right (struct [("x", enum "int" 6 9)] 0 11)
    , testCase "span excludes trailing whitespace" $
        runTestParser structType "{x: int} "
            @?= Right (struct [("x", enum "int" 4 7)] 0 8)
    , testCase "sum type field" $
        runTestParser structType "{c: Red | Blue}"
            @?= Right (struct
                [("c", sumOf [enum "Red" 4 7, enum "Blue" 10 14] 4 14)]
                0 15)
    , testCase "nested struct field" $
        runTestParser structType "{p: {x: int}}"
            @?= Right (struct
                [("p", struct [("x", enum "int" 8 11)] 4 12)]
                0 13)
    , testCase "rejects empty struct" $
        parseFails structType "{}"
    , testCase "rejects trailing comma" $
        parseFails structType "{x: int,}"
    , testCase "rejects missing colon" $
        parseFails structType "{x int}"
    , testCase "rejects missing closing brace" $
        parseFails structType "{x: int"
    ]

paramTypeTests :: TestTree
paramTypeTests =
    testGroup "paramType"
    [ testCase "single parameter" $
        runTestParser paramType "Circle(f32)"
            @?= Right (param "Circle" [enum "f32" 7 10] 0 11)
    , testCase "multiple parameters" $
        runTestParser paramType "Point(int, int)"
            @?= Right (param "Point"
                [enum "int" 6 9, enum "int" 11 14]
                0 15)
    , testCase "sum type parameter" $
        runTestParser paramType "Opt(A | B)"
            @?= Right (param "Opt" [sumOf [enum "A" 4 5, enum "B" 8 9] 4 9] 0 10)
    , testCase "span excludes trailing whitespace" $
        runTestParser paramType "Circle(f32) "
            @?= Right (param "Circle" [enum "f32" 7 10] 0 11)
    , testCase "rejects empty parameter list" $
        parseFails paramType "Point()"
    , testCase "rejects missing closing paren" $
        parseFails paramType "Point(int"
    ]

atomTests :: TestTree
atomTests =
    testGroup "atom"
    [ testCase "variable" $
        runTestParser expr "x" @?= Right (var "x" 0 1)
    , testCase "variable with digits" $
        runTestParser expr "x1y" @?= Right (var "x1y" 0 3)
    , testCase "integer" $
        runTestParser expr "42" @?= Right (int 42 0 2)
    , testCase "float" $
        runTestParser expr "3.5" @?= Right (float 3.5 0 3)
    , testCase "float beats integer on a shared prefix" $
        runTestParser expr "0.25" @?= Right (float 0.25 0 4)
    , testCase "string" $
        runTestParser expr "\"hi\"" @?= Right (str "hi" 0 4)
    , testCase "empty string" $
        runTestParser expr "\"\"" @?= Right (str "" 0 2)
    , testCase "string escape" $
        runTestParser expr "\"a\\nb\"" @?= Right (str "a\nb" 0 6)
    , testCase "string keeps its spaces" $
        runTestParser expr "\"a b\"" @?= Right (str "a b" 0 5)
    , testCase "integer followed by a dot is not a float" $
        parseFails expr "1."
    ]

precedenceTests :: TestTree
precedenceTests =
    testGroup "precedence"
    [ testCase "* binds tighter than +" $
        runTestParser expr "1 + 2 * 3"
            @?= Right (bin Plus (int 1 0 1) (bin Mul (int 2 4 5) (int 3 8 9)))
    , testCase "* binds tighter than + on the left" $
        runTestParser expr "1 * 2 + 3"
            @?= Right (bin Plus (bin Mul (int 1 0 1) (int 2 4 5)) (int 3 8 9))
    , testCase "/ binds tighter than -" $
        runTestParser expr "1 - 2 / 3"
            @?= Right (bin Minus (int 1 0 1) (bin Div (int 2 4 5) (int 3 8 9)))
    , testCase "^ binds tighter than *" $
        runTestParser expr "2 ^ 3 * 4"
            @?= Right (bin Mul (bin Pow (int 2 0 1) (int 3 4 5)) (int 4 8 9))
    , testCase "+ binds tighter than >" $
        runTestParser expr "1 + 2 > 3"
            @?= Right (bin Bt (bin Plus (int 1 0 1) (int 2 4 5)) (int 3 8 9))
    , testCase "comparison binds tighter than &&" $
        runTestParser expr "a > b && c < d"
            @?= Right (bin LogicAnd
                (bin Bt (var "a" 0 1) (var "b" 4 5))
                (bin Lt (var "c" 9 10) (var "d" 13 14)))
    , testCase "&& binds tighter than ||" $
        runTestParser expr "a && b || c && d"
            @?= Right (bin LogicOr
                (bin LogicAnd (var "a" 0 1) (var "b" 5 6))
                (bin LogicAnd (var "c" 10 11) (var "d" 15 16)))
    , testCase ">= is preferred over >" $
        runTestParser expr "a >= b"
            @?= Right (bin Bte (var "a" 0 1) (var "b" 5 6))
    , testCase "<= is preferred over <" $
        runTestParser expr "a <= b"
            @?= Right (bin Lte (var "a" 0 1) (var "b" 5 6))
    , testCase "== is a comparison" $
        runTestParser expr "a == b"
            @?= Right (bin Eq (var "a" 0 1) (var "b" 5 6))
    , testCase "!= is a comparison, not a prefix !" $
        runTestParser expr "a != b"
            @?= Right (bin Ne (var "a" 0 1) (var "b" 5 6))
    ]

associativityTests :: TestTree
associativityTests =
    testGroup "associativity"
    [ testCase "- is left associative" $
        runTestParser expr "1 - 2 - 3"
            @?= Right (bin Minus (bin Minus (int 1 0 1) (int 2 4 5)) (int 3 8 9))
    , testCase "/ is left associative" $
        runTestParser expr "1 / 2 / 3"
            @?= Right (bin Div (bin Div (int 1 0 1) (int 2 4 5)) (int 3 8 9))
    , testCase "+ is left associative" $
        runTestParser expr "1 + 2 + 3"
            @?= Right (bin Plus (bin Plus (int 1 0 1) (int 2 4 5)) (int 3 8 9))
    , testCase "^ is right associative" $
        runTestParser expr "2 ^ 3 ^ 4"
            @?= Right (bin Pow (int 2 0 1) (bin Pow (int 3 4 5) (int 4 8 9)))
    , testCase "comparisons are left associative" $
        runTestParser expr "a < b < c"
            @?= Right (bin Lt (bin Lt (var "a" 0 1) (var "b" 4 5)) (var "c" 8 9))
    , testCase "&& is left associative" $
        runTestParser expr "a && b && c"
            @?= Right (bin LogicAnd
                (bin LogicAnd (var "a" 0 1) (var "b" 5 6))
                (var "c" 10 11))
    ]

groupedTests :: TestTree
groupedTests =
    testGroup "grouped"
    [ testCase "parens override precedence" $
        runTestParser expr "(1 + 2) * 3"
            @?= Right (bin Mul
                (Spanned (sp 0 7) (EGrouped (bin Plus (int 1 1 2) (int 2 5 6))))
                (int 3 10 11))
    , testCase "parens around an atom" $
        runTestParser expr "(x)"
            @?= Right (Spanned (sp 0 3) (EGrouped (var "x" 1 2)))
    , testCase "nested parens each get a node" $
        runTestParser expr "((x))"
            @?= Right (Spanned (sp 0 5)
                (EGrouped (Spanned (sp 1 4) (EGrouped (var "x" 2 3)))))
    , testCase "span covers both parens, not the padding" $
        exprSpan "( 1 + 2 )  " @?= Right (sp 0 9)
    , testCase "rejects an empty group" $
        parseFails expr "()"
    , testCase "rejects a missing closing paren" $
        parseFails expr "(1 + 2"
    ]

-- '!' is the only prefix operator, and it builds a Neg node
unaryTests :: TestTree
unaryTests =
    testGroup "unary"
    [ testCase "prefix on an atom" $
        runTestParser expr "!x"
            @?= Right (Spanned (sp 0 2) (EUnary Neg (var "x" 1 2)))
    , testCase "prefix binds tighter than &&" $
        runTestParser expr "!a && b"
            @?= Right (bin LogicAnd
                (Spanned (sp 0 2) (EUnary Neg (var "a" 1 2)))
                (var "b" 6 7))
    , testCase "prefix on a group" $
        runTestParser expr "!(a && b)"
            @?= Right (Spanned (sp 0 9)
                (EUnary Neg (Spanned (sp 1 9)
                    (EGrouped (bin LogicAnd (var "a" 2 3) (var "b" 7 8))))))
    , testCase "space between the operator and its operand" $
        runTestParser expr "! x"
            @?= Right (Spanned (sp 0 3) (EUnary Neg (var "x" 2 3)))
    , testCase "prefix without an operand fails" $
        parseFails expr "!"
    -- unary minus is not in the operator table yet
    , testCase "leading minus is not an operator" $
        parseFails expr "-1"
    ]

exprSpanTests :: TestTree
exprSpanTests =
    testGroup "expr spans"
    [ testCase "binary span covers both operands" $
        exprSpan "1 + 2" @?= Right (sp 0 5)
    , testCase "binary span excludes trailing whitespace" $
        exprSpan "1 + 2   " @?= Right (sp 0 5)
    , testCase "binary span excludes a trailing comment" $
        exprSpan "1 + 2 // done" @?= Right (sp 0 5)
    , testCase "nested binary span reaches the outermost operands" $
        exprSpan "1 + 2 * 3 - 4" @?= Right (sp 0 13)
    , testCase "unary span starts at the operator" $
        exprSpan "!x" @?= Right (sp 0 2)
    , testCase "grouped span includes the parens" $
        exprSpan "(1)" @?= Right (sp 0 3)
    ]

exprWhitespaceTests :: TestTree
exprWhitespaceTests =
    testGroup "expr whitespace"
    [ testCase "no spaces around operators" $
        runTestParser expr "1+2*3"
            @?= Right (bin Plus (int 1 0 1) (bin Mul (int 2 2 3) (int 3 4 5)))
    , testCase "newline between operands" $
        runTestParser expr "1 +\n2"
            @?= Right (bin Plus (int 1 0 1) (int 2 4 5))
    , testCase "line comment between operands" $
        runTestParser expr "1 + // c\n2"
            @?= Right (bin Plus (int 1 0 1) (int 2 9 10))
    , testCase "block comment between operands" $
        runTestParser expr "1 /* c */ + 2"
            @?= Right (bin Plus (int 1 0 1) (int 2 12 13))
    ]

exprFailureTests :: TestTree
exprFailureTests =
    testGroup "expr failures"
    [ testCase "rejects empty input" $
        parseFails expr ""
    , testCase "rejects a dangling operator" $
        parseFails expr "1 +"
    , testCase "rejects a leading binary operator" $
        parseFails expr "* 2"
    , testCase "rejects two adjacent operands" $
        parseFails expr "1 2"
    , testCase "rejects an unclosed string" $
        parseFails expr "\"abc"
    ]

-- refType is the one place a type embeds an expression
refTypeTests :: TestTree
refTypeTests =
    testGroup "refType"
    [ testCase "refinement over an enum type" $
        runTestParser refType "n: int | n > 0"
            @?= Right (refine "n" (enum "int" 3 6)
                (bin Bt (var "n" 9 10) (int 0 13 14))
                0 14)
    , testCase "refinement without padding" $
        runTestParser refType "n:int|n>0"
            @?= Right (refine "n" (enum "int" 2 5)
                (bin Bt (var "n" 6 7) (int 0 8 9))
                0 9)
    , testCase "compound refinement predicate" $
        runTestParser refType "n: int | n > 0 && n < 10"
            @?= Right (refine "n" (enum "int" 3 6)
                (bin LogicAnd
                    (bin Bt (var "n" 9 10) (int 0 13 14))
                    (bin Lt (var "n" 18 19) (int 10 22 24)))
                0 24)
    , testCase "rejects a missing predicate" $
        parseFails refType "n: int |"
    , testCase "rejects a missing bar" $
        parseFails refType "n: int n > 0"
    ]
