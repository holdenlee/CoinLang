module Main where

import Data.List
import Data.Maybe
import Circuit
import ScriptDefs
import Compiler

std_trans::Int -> Circuit Int
std_trans pkh =
  inputs 2 .&
  ver [arg 0, arg 1] .&
  eq [con pkh, hash [arg 1]]

main::IO ()
main = do
  putStrLn (show (std_trans 999))
  putStrLn (show $ inTermsOfArgs (std_trans 999))
  putStrLn (compile (std_trans 999))
