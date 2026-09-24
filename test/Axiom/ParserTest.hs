{-# LANGUAGE OverloadedStrings #-}

module Axiom.ParserTest (tests) where

import Test.Tasty (TestTree, testGroup, localOption, mkTimeout)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Axiom.Parser
    ( Parser, ParseEnv (ParseEnv, sourceIds)
    , identifier, enumType, sumType, structType, paramType, expr
    , ifExpr, blockExpr, callExpr, listIndex, fieldAccess, range, while
    , parsePattern, listPat, tuplePat, litPat, variantPat, structPat, wildcardPat
    )
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Monad.Reader (runReader)
import Text.Megaparsec (runParserT, MonadParsec (eof))
import qualified Data.Map as Map
import Axiom.Ast
    ( Span (Span)
    , Spanned (Spanned, spanOf)
    , Type
    , TypeKind (TEnum, TParam, TStruct, TSum)
    , Expr
    , ExprKind (EAtom, EBinary, EUnary, EGrouped, EIf, EBlock, EFuncCall, EListIndex, EFieldAccess, ERange, EWhile)
    , Pattern
    , PatternKind (PBind, PList, PTuple, PStruct, POr, PLit, PVariant, PWildCard)
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
enum t s e = Spanned (sp s e) (TEnum t)

sumOf :: [Type] -> Int -> Int -> Type
sumOf ts s e = Spanned (sp s e) (TSum ts)

struct :: [(Text, Type)] -> Int -> Int -> Type
struct vars s e = Spanned (sp s e) (TStruct vars)

param :: Text -> [Type] -> Int -> Int -> Type
param i args s e = Spanned (sp s e) (TParam i args)

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

spanWith :: Parser (Spanned a) -> Text -> Either (ParseErrorBundle Text Void) Span
spanWith p input = spanOf <$> runTestParser p input

node :: a -> Int -> Int -> Spanned a
node k s e = Spanned (sp s e) k

bind :: Text -> Int -> Int -> Pattern
bind v s e = Spanned (sp s e) (PLit $ LVar v)

lit :: Atom -> Int -> Int -> Pattern
lit a s e = Spanned (sp s e) (PLit a)

wild :: Int -> Int -> Pattern
wild s e = Spanned (sp s e) PWildCard

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
    , ifExprTests
    , blockExprTests
    , callExprTests
    , listIndexTests
    , fieldAccessTests
    , rangeTests
    , whileTests
    , patternTests
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

-- none of the parsers below are reachable from expr yet, so each is run directly

ifExprTests :: TestTree
ifExprTests =
    testGroup "ifExpr"
    [ testCase "without else" $
        runTestParser ifExpr "if a then b"
            @?= Right (node (EIf (var "a" 3 4) (var "b" 10 11) Nothing) 0 11)
    , testCase "operator condition" $
        runTestParser ifExpr "if a > 0 then b"
            @?= Right (node
                (EIf (bin Bt (var "a" 3 4) (int 0 7 8)) (var "b" 14 15) Nothing)
                0 15)
    , testCase "else if chain" $
        runTestParser ifExpr "if a then b else if c then d"
            @?= Right (node
                (EIf (var "a" 3 4) (var "b" 10 11)
                    (Just (node (EIf (var "c" 20 21) (var "d" 27 28) Nothing) 17 28)))
                0 28)
    -- an if chain has to be able to end in a plain else
    , testCase "else with a plain expression" $
        runTestParser ifExpr "if a then b else c"
            @?= Right (node
                (EIf (var "a" 3 4) (var "b" 10 11) (Just (var "c" 17 18)))
                0 18)
    , testCase "span excludes trailing whitespace" $
        spanWith ifExpr "if a then b  " @?= Right (sp 0 11)
    , testCase "rejects a missing then" $
        parseFails ifExpr "if a b"
    , testCase "rejects a missing body" $
        parseFails ifExpr "if a then"
    , testCase "rejects a dangling else" $
        parseFails ifExpr "if a then b else"
    , testCase "rejects the keyword glued to an identifier" $
        parseFails ifExpr "ifa then b"
    ]

blockExprTests :: TestTree
blockExprTests =
    testGroup "blockExpr"
    [ testCase "empty block" $
        runTestParser blockExpr "{}" @?= Right (node (EBlock []) 0 2)
    , testCase "several expressions" $
        runTestParser blockExpr "{1 2}"
            @?= Right (node (EBlock [int 1 1 2, int 2 3 4]) 0 5)
    , testCase "operator expression with padding" $
        runTestParser blockExpr "{ a + b }"
            @?= Right (node (EBlock [bin Plus (var "a" 2 3) (var "b" 6 7)]) 0 9)
    , testCase "span excludes trailing whitespace" $
        spanWith blockExpr "{1}  " @?= Right (sp 0 3)
    , testCase "rejects a missing closing brace" $
        parseFails blockExpr "{1"
    ]

callExprTests :: TestTree
callExprTests =
    testGroup "callExpr"
    [ testCase "single argument" $
        runTestParser callExpr "f(x)"
            @?= Right (node (EFuncCall "f" [var "x" 2 3]) 0 4)
    , testCase "multiple arguments" $
        runTestParser callExpr "add(1, 2)"
            @?= Right (node (EFuncCall "add" [int 1 4 5, int 2 7 8]) 0 9)
    , testCase "operator argument" $
        runTestParser callExpr "f(a + b)"
            @?= Right (node (EFuncCall "f" [bin Plus (var "a" 2 3) (var "b" 6 7)]) 0 8)
    , testCase "span excludes trailing whitespace" $
        spanWith callExpr "f(x)  " @?= Right (sp 0 4)
    , testCase "rejects a missing closing paren" $
        parseFails callExpr "f(x"
    , testCase "rejects a trailing comma" $
        parseFails callExpr "f(x,)"
    ]

listIndexTests :: TestTree
listIndexTests =
    testGroup "listIndex"
    [ testCase "literal index" $
        runTestParser listIndex "xs[0]"
            @?= Right (node (EListIndex "xs" (int 0 3 4)) 0 5)
    , testCase "operator index" $
        runTestParser listIndex "xs[i + 1]"
            @?= Right (node (EListIndex "xs" (bin Plus (var "i" 3 4) (int 1 7 8))) 0 9)
    , testCase "span covers the closing bracket" $
        spanWith listIndex "xs[0]" @?= Right (sp 0 5)
    , testCase "span excludes trailing whitespace" $
        spanWith listIndex "xs[0]  " @?= Right (sp 0 5)
    , testCase "rejects an empty index" $
        parseFails listIndex "xs[]"
    , testCase "rejects a missing closing bracket" $
        parseFails listIndex "xs[0"
    ]

fieldAccessTests :: TestTree
fieldAccessTests =
    testGroup "fieldAccess"
    [ testCase "variable receiver" $
        runTestParser fieldAccess "p.x"
            @?= Right (node (EFieldAccess (var "p" 0 1) "x") 0 3)
    , testCase "grouped receiver" $
        runTestParser fieldAccess "(a).b"
            @?= Right (node (EFieldAccess (node (EGrouped (var "a" 1 2)) 0 3) "b") 0 5)
    , testCase "span excludes trailing whitespace" $
        spanWith fieldAccess "p.x  " @?= Right (sp 0 3)
    , testCase "rejects a missing dot" $
        parseFails fieldAccess "p x"
    , testCase "rejects a missing field name" $
        parseFails fieldAccess "p."
    ]

rangeTests :: TestTree
rangeTests =
    testGroup "range"
    [ testCase "integer bounds" $
        runTestParser range "1..5"
            @?= Right (node (ERange (int 1 0 1) (int 5 3 4)) 0 4)
    , testCase "padded variable bounds" $
        runTestParser range "a .. b"
            @?= Right (node (ERange (var "a" 0 1) (var "b" 5 6)) 0 6)
    , testCase "float lower bound" $
        runTestParser range "1.5..2"
            @?= Right (node (ERange (float 1.5 0 3) (int 2 5 6)) 0 6)
    , testCase "operator upper bound" $
        runTestParser range "0..n + 1"
            @?= Right (node (ERange (int 0 0 1) (bin Plus (var "n" 3 4) (int 1 7 8))) 0 8)
    , testCase "span excludes trailing whitespace" $
        spanWith range "1..5  " @?= Right (sp 0 4)
    , testCase "rejects a missing upper bound" $
        parseFails range "1.."
    , testCase "rejects a missing lower bound" $
        parseFails range "..5"
    ]

whileTests :: TestTree
whileTests =
    testGroup "while"
    [ testCase "condition and body" $
        runTestParser while "while a do b"
            @?= Right (node (EWhile (var "a" 6 7) (var "b" 11 12)) 0 12)
    , testCase "operator condition" $
        runTestParser while "while i < 10 do i"
            @?= Right (node (EWhile (bin Lt (var "i" 6 7) (int 10 10 12)) (var "i" 16 17)) 0 17)
    , testCase "rejects a missing keyword" $
        parseFails while "a b"
    , testCase "rejects a missing body" $
        parseFails while "while a"
    , testCase "rejects the keyword glued to an identifier" $
        parseFails while "whilea b"
    ]

-- the timeout keeps a looping pattern parser from hanging the whole suite
patternTests :: TestTree
patternTests =
    localOption (mkTimeout 10000) $
    testGroup "patterns"
    [ litPatTests
    , wildcardPatTests
    , tuplePatTests
    , listPatTests
    , variantPatTests
    , structPatTests
    , parsePatternTests
    ]

litPatTests :: TestTree
litPatTests =
    testGroup "litPat"
    [ testCase "integer" $
        runTestParser litPat "42" @?= Right (lit (LInt 42) 0 2)
    , testCase "float" $
        runTestParser litPat "1.5" @?= Right (lit (LFloat 1.5) 0 3)
    , testCase "string" $
        runTestParser litPat "\"ok\"" @?= Right (lit (LStr "ok") 0 4)
    , testCase "span excludes trailing whitespace" $
        runTestParser litPat "42  " @?= Right (lit (LInt 42) 0 2)
    , 
    testCase "single letter" $
        runTestParser litPat "x" @?= Right (lit (LVar "x") 0 1)
    , testCase "letters and digits" $
        runTestParser litPat "x1" @?= Right (lit (LVar "x1") 0 2)
    , testCase "span excludes trailing whitespace" $
        runTestParser litPat "x  " @?= Right (lit (LVar "x") 0 1)
    , testCase "rejects a leading digit" $
        parseFails litPat "1x"
    ]

wildcardPatTests :: TestTree
wildcardPatTests =
    testGroup "wildcardPat"
    [ testCase "underscore" $
        runTestParser wildcardPat "_" @?= Right (wild 0 1)
    , testCase "span excludes trailing whitespace" $
        runTestParser wildcardPat "_  " @?= Right (wild 0 1)
    , testCase "rejects an underscore-prefixed name" $
        parseFails wildcardPat "_x"
    ]

tuplePatTests :: TestTree
tuplePatTests =
    testGroup "tuplePat"
    [ testCase "empty tuple" $
        runTestParser tuplePat "()" @?= Right (node (PTuple []) 0 2)
    , testCase "two elements" $
        runTestParser tuplePat "(a, b)"
            @?= Right (node (PTuple [bind "a" 1 2, bind "b" 4 5]) 0 6)
    , testCase "nested tuple" $
        runTestParser tuplePat "(a, (b, c))"
            @?= Right (node
                (PTuple [bind "a" 1 2, node (PTuple [bind "b" 5 6, bind "c" 8 9]) 4 10])
                0 11)
    , testCase "span excludes trailing whitespace" $
        spanWith tuplePat "(a, b)  " @?= Right (sp 0 6)
    , testCase "rejects a missing closing paren" $
        parseFails tuplePat "(a, b"
    ]

listPatTests :: TestTree
listPatTests =
    testGroup "listPat"
    [ testCase "empty list" $
        runTestParser listPat "[]" @?= Right (node (PList []) 0 2)
    , testCase "binding and wildcard" $
        runTestParser listPat "[a, _]"
            @?= Right (node (PList [lit (LVar "a") 1 2, wild 4 5]) 0 6)
    , testCase "literal and binding" $
        runTestParser listPat "[1, x]"
            @?= Right (node (PList [lit (LInt 1) 1 2, bind "x" 4 5]) 0 6)
    , testCase "rejects a trailing comma" $
        parseFails listPat "[a,]"
    ]

variantPatTests :: TestTree
variantPatTests =
    testGroup "variantPat"
    [ testCase "single field" $
        runTestParser variantPat "Some(x)"
            @?= Right (node (PVariant "Some" [bind "x" 5 6]) 0 7)
    , testCase "multiple fields" $
        runTestParser variantPat "Point(x, _)"
            @?= Right (node (PVariant "Point" [bind "x" 6 7, wild 9 10]) 0 11)
    , testCase "rejects a missing field list" $
        parseFails variantPat "Some"
    ]

structPatTests :: TestTree
structPatTests =
    testGroup "structPat"
    [ testCase "empty struct" $
        runTestParser structPat "{}" @?= Right (node (PStruct []) 0 2)
    , testCase "single field" $
        runTestParser structPat "{x: a}"
            @?= Right (node (PStruct [("x", bind "a" 4 5)]) 0 6)
    , testCase "multiple fields" $
        runTestParser structPat "{x: a, y: _}"
            @?= Right (node (PStruct [("x", bind "a" 4 5), ("y", wild 10 11)]) 0 12)
    , testCase "nested list pattern with padding" $
        runTestParser structPat "{ x: [a, b] }"
            @?= Right (node
                (PStruct [("x", node (PList [bind "a" 6 7, bind "b" 9 10]) 5 11)])
                0 13)
    , testCase "rejects a missing colon" $
        parseFails structPat "{x a}"
    ]

-- parsePattern owns the or-level and has to pick the right alternative for each shape
parsePatternTests :: TestTree
parsePatternTests =
    testGroup "parsePattern"
    [ testCase "literal alternatives" $
        runTestParser parsePattern "400 | 403"
            @?= Right (node (POr [lit (LInt 400) 0 3, lit (LInt 403) 6 9]) 0 9)
    , testCase "alternatives without spaces" $
        runTestParser parsePattern "a|b|c"
            @?= Right (node (POr [bind "a" 0 1, bind "b" 2 3, bind "c" 4 5]) 0 5)
    , testCase "variant alternatives" $
        runTestParser parsePattern "A(x) | B(y)"
            @?= Right (node
                (POr [node (PVariant "A" [bind "x" 2 3]) 0 4, node (PVariant "B" [bind "y" 9 10]) 7 11])
                0 11)
    , testCase "or span excludes trailing whitespace" $
        spanWith parsePattern "a | b  " @?= Right (sp 0 5)
    , testCase "rejects a trailing bar" $
        parseFails parsePattern "a |"
    -- mirrors sumType: a single alternative is the pattern itself
    , testCase "binding" $
        runTestParser parsePattern "x" @?= Right (bind "x" 0 1)
    , testCase "literal" $
        runTestParser parsePattern "42" @?= Right (lit (LInt 42) 0 2)
    , testCase "wildcard" $
        runTestParser parsePattern "_" @?= Right (wild 0 1)
    , testCase "variant is not a binding" $
        runTestParser parsePattern "Some(x)"
            @?= Right (node (PVariant "Some" [bind "x" 5 6]) 0 7)
    , testCase "tuple" $
        runTestParser parsePattern "(a, b)"
            @?= Right (node (PTuple [bind "a" 1 2, bind "b" 4 5]) 0 6)
    , testCase "struct" $
        runTestParser parsePattern "{x: a}"
            @?= Right (node (PStruct [("x", bind "a" 4 5)]) 0 6)
    , testCase "or pattern inside a list" $
        runTestParser parsePattern "[1 | 2, _]"
            @?= Right (node
                (PList [node (POr [lit (LInt 1) 1 2, lit (LInt 2) 5 6]) 1 6, wild 8 9])
                0 10)
    ]
