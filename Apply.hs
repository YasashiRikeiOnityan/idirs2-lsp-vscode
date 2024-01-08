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
