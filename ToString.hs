module ToString where

import AST

signature2str :: Signature -> String
signature2str (Signature n Colon te) = n ++ " : " ++ typeexpr2str te

typeexpr2str :: TypeExpr -> String
typeexpr2str (Single f) = factor2str f
typeexpr2str (Function f To te) = factor2str f ++ " -> " ++ typeexpr2str te
typeexpr2str (Constrained f RightArrow te) = factor2str f ++ " => " ++ typeexpr2str te

factor2str :: Factor -> String
factor2str (SignatureFactor LPar s RPar) = "(" ++ signature2str s ++ ")"
factor2str (TypeExprFactor LPar te RPar) = "(" ++ typeexpr2str te ++ ")"
factor2str (TypeNameFactor LPar tn RPar) = "(" ++ typename2str tn ++ ")"
factor2str (JustTypeName tn) = typename2str tn

typename2str :: TypeName -> String
typename2str (TypeName n) = n
typename2str (TypeWithModifier n tn) = n ++ " " ++ typename2str tn
typename2str (TNCons tnf) = tnfactor2str tnf
typename2str (TypeWithTNCons tnf tn) = tnfactor2str tnf ++ " " ++ typename2str tn

tnfactor2str :: TNFactor -> String
tnfactor2str (TNFactor LPar tn RPar) = "(" ++ typename2str tn ++ ")"

toStrtest = signature2str $ Signature "insElm" Colon (Constrained (TypeExprFactor LPar (Single (JustTypeName (TypeWithModifier "Ord" (TypeName "a")))) RPar) RightArrow (Function (JustTypeName (TypeName "a")) To (Function (JustTypeName (TypeWithModifier "Vect" (TypeWithModifier "len" (TypeName "a")))) To (Single (JustTypeName (TypeWithModifier "Vect" (TypeWithTNCons (TNFactor LPar (TypeWithModifier "S" (TypeName "len")) RPar) (TypeName "a"))))))))
