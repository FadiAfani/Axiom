{-# LANGUAGE OverloadedStrings #-}

module Axiom.ParserTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Axiom.Parser (Parser, ParseEnv (ParseEnv, sourceIds), identifier, sumType, structType, paramType)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Monad.Reader (runReader)
import Text.Megaparsec (runParserT, MonadParsec (eof))
import qualified Data.Map as Map
import Axiom.Ast (Identifier (Identifier), StructType (StructType), ParamType (ParamType), Span (Span), AxiomType (TSum), SumType (SumType), AtomicType (TEnum, TStruct))

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

ident :: Text -> Int -> Int -> Identifier
ident t s e = Identifier t (sp s e)

enum :: Text -> Int -> Int -> AtomicType
enum t s e = TEnum (ident t s e)

sumOf :: [AtomicType] -> Int -> Int -> AxiomType
sumOf ts s e = TSum (SumType ts (sp s e))

tests :: TestTree
tests =
    testGroup "Parser"
    [ identifierTests
    , sumTypeTests
    , structTypeTests
    , paramTypeTests
    ]

identifierTests :: TestTree
identifierTests =
    testGroup "identifier"
    [ testCase "single letter" $
        runTestParser identifier "x" @?= Right (ident "x" 0 1)
    , testCase "letters and digits" $
        runTestParser identifier "abc123" @?= Right (ident "abc123" 0 6)
    , testCase "interleaved letters and digits" $
        runTestParser identifier "x1y" @?= Right (ident "x1y" 0 3)
    , testCase "span excludes trailing whitespace (single letter)" $
        runTestParser identifier "x  " @?= Right (ident "x" 0 1)
    , testCase "span excludes trailing whitespace (multi letter)" $
        runTestParser identifier "int  " @?= Right (ident "int" 0 3)
    , testCase "rejects whitespace inside the name" $
        parseFails identifier "ab cd"
    , testCase "rejects leading digit" $
        parseFails identifier "1x"
    , testCase "rejects empty input" $
        parseFails identifier ""
    ]

sumTypeTests :: TestTree
sumTypeTests =
    testGroup "sumType"
    [ testCase "single variant" $
        runTestParser sumType "int" @?= Right (SumType [enum "int" 0 3] (sp 0 3))
    , testCase "multiple variants" $
        runTestParser sumType "A | B | C"
            @?= Right (SumType [enum "A" 0 1, enum "B" 4 5, enum "C" 8 9] (sp 0 9))
    , testCase "variants without spaces" $
        runTestParser sumType "Red|Green"
            @?= Right (SumType [enum "Red" 0 3, enum "Green" 4 9] (sp 0 9))
    , testCase "comments between variants" $
        runTestParser sumType "A // c\n| B"
            @?= Right (SumType [enum "A" 0 1, enum "B" 9 10] (sp 0 10))
    , testCase "struct variant" $
        runTestParser sumType "{x: int} | B"
            @?= Right (SumType
                [ TStruct $ StructType [(ident "x" 1 2, sumOf [enum "int" 4 7] 4 7)] (sp 0 8)
                , enum "B" 11 12
                ] (sp 0 12))
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
            @?= Right (StructType [(ident "x" 1 2, sumOf [enum "int" 4 7] 4 7)] (sp 0 8))
    , testCase "multiple fields" $
        runTestParser structType "{x: int, y: int}"
            @?= Right (StructType
                [ (ident "x" 1 2, sumOf [enum "int" 4 7] 4 7)
                , (ident "y" 9 10, sumOf [enum "int" 12 15] 12 15)
                ] (sp 0 16))
    , testCase "whitespace inside braces" $
        runTestParser structType "{ x : int }"
            @?= Right (StructType [(ident "x" 2 3, sumOf [enum "int" 6 9] 6 9)] (sp 0 11))
    , testCase "span excludes trailing whitespace" $
        runTestParser structType "{x: int} "
            @?= Right (StructType [(ident "x" 1 2, sumOf [enum "int" 4 7] 4 7)] (sp 0 8))
    , testCase "sum type field" $
        runTestParser structType "{c: Red | Blue}"
            @?= Right (StructType
                [(ident "c" 1 2, sumOf [enum "Red" 4 7, enum "Blue" 10 14] 4 14)]
                (sp 0 15))
    , testCase "nested struct field" $
        runTestParser structType "{p: {x: int}}"
            @?= Right (StructType
                [(ident "p" 1 2, sumOf
                    [TStruct $ StructType [(ident "x" 5 6, sumOf [enum "int" 8 11] 8 11)] (sp 4 12)]
                    4 12)]
                (sp 0 13))
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
            @?= Right (ParamType (ident "Circle" 0 6) [sumOf [enum "f32" 7 10] 7 10] (sp 0 11))
    , testCase "multiple parameters" $
        runTestParser paramType "Point(int, int)"
            @?= Right (ParamType (ident "Point" 0 5)
                [sumOf [enum "int" 6 9] 6 9, sumOf [enum "int" 11 14] 11 14]
                (sp 0 15))
    , testCase "sum type parameter" $
        runTestParser paramType "Opt(A | B)"
            @?= Right (ParamType (ident "Opt" 0 3) [sumOf [enum "A" 4 5, enum "B" 8 9] 4 9] (sp 0 10))
    , testCase "span excludes trailing whitespace" $
        runTestParser paramType "Circle(f32) "
            @?= Right (ParamType (ident "Circle" 0 6) [sumOf [enum "f32" 7 10] 7 10] (sp 0 11))
    , testCase "rejects empty parameter list" $
        parseFails paramType "Point()"
    , testCase "rejects missing closing paren" $
        parseFails paramType "Point(int"
    ]
