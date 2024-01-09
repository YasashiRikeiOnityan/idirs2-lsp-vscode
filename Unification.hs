module Unification where

import AST

makeName :: Name -> Name -> Name
makeName x y = x ++ " " ++ y

unifySignatures :: [Signature] -> Signature
unifySignatures = foldl1 unifySignature

unifySignature :: Signature -> Signature -> Signature
unifySignature (Signature x Colon (Function f1 To te)) (Signature y Colon (Single f2)) 
    = Signature (makeName x y) Colon te
unifySignature (Signature x Colon (Function f1 To te1)) (Signature y Colon (Function f2 To te2)) 
    = Signature (makeName x y) Colon te1
unifySignature (Signature x Colon (Constrained f1 RightArrow (Function f2 To te))) (Signature y Colon (Single f3))
    = Signature (makeName x y) Colon te
unifySignature (Signature x Colon (Constrained f1 RightArrow (Function f2 To te1))) (Signature y Colon (Function f3 To te2)) 
    = Signature (makeName x y) Colon te1
