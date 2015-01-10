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
  --putStrLn ("Circuit: "++(show c))
  --putStrLn ("Circuit replaced: "++(show $ replaceArgs c))
  --putStrLn ("Circuit in terms of args: "++(show $ inTermsOfArgs $ replaceArgs c))
  --putStrLn ("Var bindings"++((\(x,y) -> show y) c))
  putStrLn (compile c)

main::IO ()
main = test >> test2 >> test3 >> test4 >> test5
{-
Should give
OP_DUP OP_HASH160 999 OP_EQUALVERIFY OP_CHECKSIG
999 f
2 OP_ROLL 2 OP_ROLL f 999 1 OP_PICK g
f g
2 OP_ROLL 1 OP_ROLL OP_DUP OP_HASH160 999 OP_EQUALVERIFY OP_CHECKSIG 1 OP_ROLL g
-}

test::IO ()
test = testC $ std_trans 999

f = makeFun "f" True
g = makeFun "g" False

f2::Int -> Circuit2 Int
f2 z =
  inputs 1 .&
  f [con z, arg 0]

test2:: IO()
test2 = testC (f2 999)

f3::Int -> Circuit2 Int
f3 z =
  inputs 3 .&
  f [arg 0, arg 1] .&
  set "a" .&
  g [var "a", con z, var "a"]

test3 = testC (f3 999)

f4::Circuit2 Int
f4 = 
    inputs 1 .&
    g [f [arg 0]]

test4 = testC f4

std_trans_f = fun (std_trans 999)

f5::Circuit2 Int
f5 = 
    inputs 3 .&
    g [std_trans_f [arg 0, arg 2], arg 1]

test5 = testC f5
