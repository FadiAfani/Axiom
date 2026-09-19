{-# LANGUAGE OverloadedStrings #-}

module Axiom.ParserTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Axiom.Parser (Parser, ParseEnv (ParseEnv, sourceIds), structType)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Monad.Reader (runReader)
import Text.Megaparsec (runParserT, MonadParsec (eof))
import qualified Data.Map as Map
import Axiom.Ast (Identifier (Identifier), StructType (StructType, typedVars, structTSpan), Span (Span), AxiomType (TSum), SumType (SumType), AtomicType (TEnum))

testEnv :: ParseEnv
testEnv = ParseEnv {
    sourceIds = Map.fromList [("", 0)]
}

runTestParser :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runTestParser p input = runReader (runParserT (p <* eof) "" input) testEnv

-- "{x: int, y: int}"
--  0123456789012345
expected :: StructType
expected = StructType {
    typedVars = [
        (Identifier "x" $ Span 0 1 2, TSum $ SumType [TEnum $ Identifier "int" $ Span 0 4 7] (Span 0 4 7)),
        (Identifier "y" $ Span 0 9 10, TSum $ SumType [TEnum $ Identifier "int" $ Span 0 12 15] (Span 0 12 15))
    ],
    structTSpan = Span 0 0 16
}

tests :: TestTree
tests =
    testGroup "Parser"
    [
        testCase "struct type" $
        runTestParser structType "{x: int, y: int}" @?= Right expected
    ]
