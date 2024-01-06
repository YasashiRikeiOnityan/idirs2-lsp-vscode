module Apply where

import AST
import Constraints

applyConstrainsSignature :: Constraints -> Signature -> Signature
applyConstrainsSignature c (Signature n Colon te) = Signature n Colon (applyConstrainsTypeExpr c te)

applyConstrainsTypeExpr :: Constraints -> TypeExpr -> TypeExpr
applyConstrainsTypeExpr c (Single f) = Single (applyConstrainsFactor c f)
applyConstrainsTypeExpr c (Function f To te) = Function (applyConstrainsFactor c f) To (applyConstrainsTypeExpr c te)
applyConstrainsTypeExpr c (Constrained f RightArrow te) = Constrained f RightArrow (applyConstrainsTypeExpr c te)

applyConstrainsFactor :: Constraints -> Factor -> Factor
applyConstrainsFactor c f = let constrain = filter (\f' -> fst f' == f) c
    in if null constrain then f else snd $ head constrain

applyTest = applyConstrainsSignature [(JustTypeName (TypeName "a"),TypeExprFactor LPar (Function (JustTypeName (TypeName "Nat")) To (Single (JustTypeName (TypeName "Nat")))) RPar)] (Signature "add" Colon (Constrained (JustTypeName (TypeWithModifier "Ord" (TypeName "a"))) RightArrow (Function (JustTypeName (TypeName "a")) To (Function (JustTypeName (TypeName "a")) To (Single (JustTypeName (TypeName "a")))))))
-- Signature "add" Colon (Constrained (JustTypeName (TypeWithModifier "Ord" (TypeName "a"))) RightArrow (Function (TypeExprFactor LPar (Function (JustTypeName (TypeName "Nat")) To (Single (JustTypeName (TypeName "Nat")))) RPar) To (Function (TypeExprFactor LPar (Function (JustTypeName (TypeName "Nat")) To (Single (JustTypeName (TypeName "Nat")))) RPar) To (Single (TypeExprFactor LPar (Function (JustTypeName (TypeName "Nat")) To (Single (JustTypeName (TypeName "Nat")))) RPar)))))
