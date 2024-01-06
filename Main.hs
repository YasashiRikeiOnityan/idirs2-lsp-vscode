module Main where

import AST
import Parser
import Constraints
import ToString
import System.Environment (getArgs)

main :: IO()
main = do input <- getArgs
          print ""
         
