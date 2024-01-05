module AST where

type Name = String

data Colon = Colon deriving (Show, Eq)

data To = To deriving (Show, Eq)

data RightArrow = RightArrow deriving (Show, Eq)

data LPar = LPar deriving (Show, Eq)

data RPar = RPar deriving (Show, Eq)

data Signature = Signature Name Colon TypeExpr
    deriving (Show, Eq)

data TypeExpr = Single Factor
              | Function Factor To TypeExpr
              | Constrained Factor RightArrow TypeExpr
    deriving (Show, Eq)

data TypeName = TypeName Name
              | TypeWithModifier Name TypeName
              | TNCons TNFactor
              | TypeWithTNCons TNFactor TypeName
    deriving (Show, Eq)

data Factor = SignatureFactor LPar Signature RPar
            | TypeExprFactor LPar TypeExpr RPar
            | TypeNameFactor LPar TypeName RPar
            | JustTypeName TypeName
    deriving (Show, Eq)

data TNFactor = TNFactor LPar TypeName RPar
    deriving (Show, Eq)
