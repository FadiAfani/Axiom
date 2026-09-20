{-# LANGUAGE OverloadedStrings #-}

module Axiom.ParserTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Axiom.Parser (Parser, ParseEnv (ParseEnv, sourceIds), identifier, enumType, sumType, structType, paramType)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Monad.Reader (runReader)
import Text.Megaparsec (runParserT, MonadParsec (eof))
import qualified Data.Map as Map
import Axiom.Ast (Span (Span), Type (Type), TypeKind (TEnum, TParam, TStruct, TSum))

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

tests :: TestTree
tests =
    testGroup "Parser"
    [ identifierTests
    , enumTypeTests
    , sumTypeTests
    , structTypeTests
    , paramTypeTests
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
