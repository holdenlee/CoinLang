module Main where

import Data.List
import Data.Maybe
import Circuit
import ScriptDefs

std_trans::Int -> Circuit2 Int
std_trans pkh =
  inputs 2 .&
  ver [arg 0, arg 1] .&
  eq [con pkh, hash [arg 1]]

f = makeFun "f" False
g = makeFun "g" True

test::Int -> Circuit2 Int
test z =
  inputs 2 .&
  f [arg 0, arg 1] .&
  set "w" .&
  g [var "w", var "w", con z]

main::IO ()
main = do
  putStrLn (show (std_trans 999))
  putStrLn (show $ inTermsOfArgs $ replaceArgs (std_trans 999))
  putStrLn (show (test 999))
  putStrLn (show $ inTermsOfArgs $ replaceArgs (test 999))

  
