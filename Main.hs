module Main where

import AST
import Parser
import Unification
import Constraints
import Apply
import ToString
import System.Environment (getArgs)

parseErrorSig :: Signature
parseErrorSig = Signature "error" Colon (Single (JustTypeName (TypeName "parsing failed")))

main :: IO()
main = do input <- getArgs
          let sigs = map parse2signature input
          if parseErrorSig `elem` sigs
              then print $ signature2str parseErrorSig
              else do let unified = unifySignatures sigs
                      let constraints = getConstraints [] sigs
                      let output = signature2str $ applyConstrainsSignature constraints unified
                      print output
