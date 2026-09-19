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

data ParamType = ParamType {
    const :: Identifier,
    params :: [AxiomType],
    varTSpan :: Span
} deriving (Show, Eq)

data SumType = SumType {
    types :: [AtomicType],
    sumTSpan :: Span
} deriving (Show, Eq)

data RefinementType = RefinementType {
    varName :: Identifier,
    typeName :: AxiomType,
    pred :: Expr,
    refTSpan :: Span
} deriving (Show, Eq)

data StructType = StructType {
    typedVars :: [(Identifier, AxiomType)],
    structTSpan :: Span
} deriving (Show, Eq)

data TypeBody = EnumType Identifier
    | TupleType Text [Text]

-- type Enum = Blue | Orange
-- type Tuple = Integer(string) | Double(string)
-- type Point = { x: int, y: int }
-- type Nat = { n: int | n > 0 }

data AtomicType = TRef RefinementType
    |   TStruct StructType
    | TEnum Identifier deriving (Show, Eq)

data AxiomType = TSum SumType deriving (Show, Eq)
