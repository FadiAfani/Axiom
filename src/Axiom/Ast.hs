{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Axiom.Ast where
import Data.Text (Text)

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

instance HasSpan Span       where spanOf = id
instance HasSpan Identifier where spanOf = idSpan
instance HasSpan Type       where spanOf = typeSpan
instance HasSpan Expr       where spanOf = exprSpan

data Identifier = Identifier {
    idName :: !Text,
    idSpan :: !Span
} deriving (Show, Eq)

-- type Enum = Blue | Orange
-- type Tuple = Integer(string) | Double(string)
-- type Point = { x: int, y: int }
-- type Nat = { n: int | n > 0 }

data Type = Type {
    typeKind :: TypeKind,
    typeSpan :: Span
} deriving (Show, Eq)

data TypeKind = TEnum Identifier
    | TParam Identifier [Type]
    | TStruct [(Identifier, Type)]
    | TRefine Identifier Type Expr
    | TSum [Type] deriving (Show, Eq)

data Expr = Expr {
    exprKind :: ExprKind,
    exprSpan :: Span
} deriving (Show, Eq)

data ExprKind = EAtom Atom
    | BinExpr Op Expr Expr
    | UnaryExpr Op Expr deriving (Show, Eq)

data Atom = Var Identifier
    | StrLit Text
    | IntLit Int
    | FloatLit Double deriving (Show, Eq)

data Op = Plus | Minus | Mul | Div deriving (Show, Eq)
