module Main where

import Parser
import Unification
import Constraints
import Apply
import ToString
import System.Environment (getArgs)

main :: IO()
main = do input <- getArgs
          let sigs = map parse2signature input
          let unified = unifySignatures sigs
          let constraints = getConstraints [] sigs
          let output = signature2str $ applyConstrainsSignature constraints unified
          print output
         
