module Main where

import Data.List
import Data.Maybe
import Circuit
import ScriptDefs
import Compiler

std_trans::Int -> Circuit2 Int
std_trans pkh =
  inputs 2 .&
  ver [arg 0, arg 1] .&
  eq [con pkh, hash [arg 1]]

testC::Circuit2 Int -> IO ()
testC c = do
  putStrLn (show c)
  putStrLn (show $ inTermsOfArgs $ replaceArgs c)
  putStrLn (compile c)

main::IO ()
main = test2

test::IO ()
test = testC $ std_trans 999

f = makeFun "f" True

f2::Int -> Circuit2 Int
f2 z =
  inputs 1 .&
  f [con z, arg 0]

test2:: IO()
test2 = testC (f2 999)
