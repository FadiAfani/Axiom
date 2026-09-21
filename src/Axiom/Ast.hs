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

data Spanned a = Spanned {
    spanOf :: !Span,
    spanVal :: !a
} deriving (Show, Eq)

-- names are bare Text: the enclosing node's span locates them

-- type Enum = Blue | Orange
-- type Tuple = Integer(string) | Double(string)
-- type Point = { x: int, y: int }
-- type Nat = { n: int | n > 0 }


data Type = Type {
    typeKind :: TypeKind,
    typeSpan :: Span
} deriving (Show, Eq)

data TypeKind = TEnum Text
    | TParam Text [Type]
    | TStruct [(Text, Type)]
    | TRefine Text Type Expr
    | TSum [Type] deriving (Show, Eq)

type Expr = Spanned ExprKind
type Stmt = Spanned StmtKind


-- x
-- [x, y, { a, b }]
-- { x, y }, { x: [a, b] }
-- 400 | 403
data Pattern = PBind Text 
    | PList [Pattern]
    | PTuple [Pattern]
    | PStruct Text [(Text, Pattern)]
    | POr [Pattern]
    | PLit Atom
    | PVariant Text [Pattern]
    | PWildCard
    deriving (Show, Eq)

data StmtKind = SModule Text
    deriving (Show, Eq)


data ExprKind = EAtom Atom
    | EBinary BinOp Expr Expr
    | EUnary UnOp Expr 
    | EGrouped Expr 
    | EIf Expr Expr (Maybe Expr)
    | EBlock [Expr]
    | EFuncCall Text [Expr]
    | EListIndex Text Expr
    | EFieldAccess Expr Text
    | ERange Expr Expr 
    | EFor Pattern Expr
    | EWhile Expr Expr
    deriving (Show, Eq)

data Atom = LVar Text
    | LStr Text
    | LInt Int
    | LFloat Double 
    | LPtrDeRef Text
    deriving (Show, Eq)

data BinOp = Plus | Minus | Mul | Div | Bt | Bte | Lt | Lte | Eq | LogicOr | LogicAnd | Ne | Pow deriving (Show, Eq)
data UnOp = Neg deriving (Show, Eq)
