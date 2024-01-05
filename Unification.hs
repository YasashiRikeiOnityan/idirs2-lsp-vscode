module Unification where

import AST
import Parser

type Constraints = [(Factor, Factor)]

makeName :: Name -> Name -> Name
makeName x y = x ++ " " ++ y

unifySignatures :: [Signature] -> Signature
unifySignatures = foldl1 unifySignature

unifySignature :: Signature -> Signature -> Signature
unifySignature (Signature x Colon (Function f1 To te)) (Signature y Colon (Single f3)) 
    = Signature (makeName x y) Colon te
unifySignature (Signature x Colon (Constrained f1 RightArrow te)) (Signature y Colon (Single f3))
    = Signature (makeName x y) Colon te

typeexprConstrains :: Constraints -> TypeExpr -> TypeExpr -> Constraints
typeexprConstrains c (Single f1) (Single f2) = factorConstraints c f1 f2
typeexprConstrains c (Single f1) (Function f2 To _) = factorConstraints c f1 f2
typeexprConstrains c (Single f1) (Constrained _ RightArrow (Single f2)) = factorConstraints c f1 f2
typeexprConstrains c (Single f1) (Constrained _ RightArrow (Function f2 To _)) = factorConstraints c f1 f2
typeexprConstrains c (Function f1 To _) (Single f2) = factorConstraints c f1 f2
typeexprConstrains c (Constrained f1 RightArrow _) (Single f2) = factorConstraints c f1 f2

factorConstraints :: Constraints -> Factor -> Factor -> Constraints
factorConstraints c f1 f2
    | f1 == f2  = c
    | otherwise = c ++ [(f1, f2)]

unifyTest = unifySignatures [Signature "add" Colon (Function (JustTypeName (TypeName "Nat")) To (Function (JustTypeName (TypeName "Nat")) To (Single (JustTypeName (TypeName "Nat"))))),
                             Signature "x" Colon (Single (JustTypeName (TypeName "Nat")))]