{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Axiom.Ast where
import Data.Text (Text)

data Atom = Var Text
    | StrLit Text
    | IntLit Int
    | FloatLit Double deriving (Show, Eq)

data Expr = AtomExpr Atom deriving (Show, Eq)

data Span = Span {
    srcIdx :: {-# UNPACK #-} !Int,
    startPos :: {-# UNPACK #-} !Int,
    endPos :: {-# UNPACK #-} !Int
} deriving (Show, Eq)

data Identifier = Identifier {
    idName :: !Text,
    idSpan :: !Span
} deriving (Show, Eq)

data VariantType = VariantType {
    const :: Identifier,
    params :: [AxiomType],
    varTSpan :: Span
} deriving (Show, Eq)

data RefinementType = RefinementType {
    varName :: Identifier,
    typeName :: Identifier,
    pred :: [Expr],
    refTSpan :: Span
} deriving (Show, Eq)

data StructType = StructType {
    varNames :: [Identifier],
    varTypes :: [Identifier],
    structTSpan :: Span
} deriving (Show, Eq)

data TypeBody = EnumType Identifier
    | TupleType Text [Text]

-- type Enum = Blue | Orange
-- type Tuple = Integer(string) | Double(string)
-- type Point = { x: int, y: int }
-- type Nat = { n: int | n > 0 }
data AxiomType = TRef RefinementType
    | TVar VariantType
    | StructT StructType
    | EnumT Identifier deriving (Show, Eq)
