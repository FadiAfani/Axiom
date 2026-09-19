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

instance Semigroup Span where
    (<>) :: Span -> Span -> Span
    Span src s1 e1 <> Span _ s2 e2 = Span src (min s1 s2) (max e1 e2)

class HasSpan a where
    spanOf :: a -> Span

instance HasSpan Span           where spanOf = id
instance HasSpan Identifier     where spanOf = idSpan
instance HasSpan StructType     where spanOf = structTSpan
instance HasSpan SumType        where spanOf = sumTSpan
instance HasSpan ParamType      where spanOf = varTSpan
instance HasSpan RefinementType where spanOf = refTSpan

instance HasSpan AtomicType where
    spanOf (TEnum i)   = spanOf i
    spanOf (TStruct s) = spanOf s
    spanOf (TRef r)    = spanOf r

instance HasSpan AxiomType where
    spanOf (TSum s) = spanOf s 

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
