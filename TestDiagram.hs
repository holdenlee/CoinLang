module Main where

import Data.List
import Data.Maybe

import Utilities
import CircuitViz
import Circuit
import ScriptDefs
import Compiler
import Verify
import MultipartyLottery
import CircuitViz

main = do 
  let verifyG = generateDotGraph $ verify' 999
  let compute2G = generateDotGraph $ compute2' 9 11 22 111 222
  writeFile "verify.dot" verifyG
  writeFile "compute2.dot" compute2G
  --let c = computeN' 9 [11,22,33] [111,222,333]
