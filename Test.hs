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

main :: IO ()
main = foldl1 (>>) $ map testC [verify' 999, f2 999, f3 999, f4, f5, putMoneyB' 9 11 111, compute2' 9 11 22 111 222]
{-
Gives
OP_DUP OP_HASH160 999 OP_EQUALVERIFY OP_CHECKSIG
999 f
0 OP_ROLL OP_DROP f 999 1 OP_PICK g
f g
2 OP_ROLL 1 OP_ROLL OP_DUP OP_HASH160 999 OP_EQUALVERIFY OP_CHECKSIG 1 OP_ROLL g
OP_DUP OP_SIZE OP_NIP OP_DUP 72 81 OP_WITHIN OP_VERIFY 8 OP_MOD 0 OP_EQUALVERIFY OP_HASH256 111 OP_EQUALVERIFY 11 OP_CHECKSIG
OP_DUP OP_SIZE OP_NIP OP_DUP 72 81 OP_WITHIN OP_VERIFY 8 OP_MOD 0 OP_EQUAL 1 OP_PICK OP_HASH256 222 OP_EQUAL 3 OP_PICK 3 OP_PICK OP_SUM 2 OP_MOD OP_DUP 0 OP_EQUAL 1 OP_ROLL 1 OP_EQUAL 5 OP_PICK 5 OP_ROLL OP_SUM 2 OP_MOD OP_VERIFY 4 OP_PICK OP_SIZE OP_NIP OP_DUP 72 81 OP_WITHIN OP_VERIFY 8 OP_MOD 0 OP_EQUAL 5 OP_ROLL OP_HASH256 111 OP_EQUAL 4 OP_ROLL OP_BOOLAND 4 OP_ROLL OP_BOOLAND OP_BOOLAND 3 OP_PICK 22 OP_CHECKSIG 2 OP_ROLL 1 OP_PICK OP_BOOLAND 4 OP_ROLL 22 OP_CHECKSIGVERIFY 4 OP_PICK 11 OP_CHECKSIG OP_DUP 3 OP_ROLL OP_BOOLAND 4 OP_ROLL 2 OP_ROLL OP_BOOLAND 2 OP_ROLL OP_BOOLOR 2 OP_ROLL OP_BOOLAND OP_BOOLOR OP_VERIFY 11 OP_CHECKSIG
-}

testC::Circuit2 Int -> IO ()
testC c = putStrLn (compile c)

verboseTestC :: Circuit2 Int -> IO ()
verboseTestC c = do
  putStrLn ("Circuit: "++(show c))
  putStrLn ("Circuit replaced: "++(show $ replaceArgs c))
  let c' =  inTermsOfArgs $ replaceArgs c
  putStrLn ("Circuit in terms of args: "++(show c'))
  let ins = map (\x -> case x of Fun s _ li -> map removeArg li
                                 _ -> []
                 ) c'
  putStrLn ("dot graph: "++ (c' |> (circToGraph ins) |> graphToDot))
  putStrLn ("Var bindings"++((\(x,y) -> show y) c))
  putStrLn (compile c)

f = makeFun "f" True
g = makeFun "g" False

f2::Int -> Circuit2 Int
f2 z =
  inputs 1 .>
  f [con z, arg 0]

f3::Int -> Circuit2 Int
f3 z =
  inputs 3 .>
  f [arg 0, arg 1] .>
  set "a" .>
  g [var "a", con z, var "a"]

f4::Circuit2 Int
f4 = 
    inputs 1 .>
    g [f [arg 0]]

f5::Circuit2 Int
f5 = 
    inputs 3 .>
    g [(verify 999) [arg 0, arg 2], arg 1]
