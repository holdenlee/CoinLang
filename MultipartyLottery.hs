module Main where

import Data.List
import Data.Maybe
import Circuit
import ScriptDefs
import Compiler
import CircuitViz
import Utilities

std_trans::Int -> Circuit2 Int
std_trans pkh =
  inputs 2 .>
  ver [arg 0, arg 1] .>
  eq [con pkh, hash [arg 1]]

std_trans_f x = makeFun "OP_CHECKSIG" False
--fun (std_trans x)

{-
commit x pk_c pk_p = 
 script [
  inputs 4,
  ifelse (arg 3)
         (script [
           h [arg 3] .= h,
           ver [arg 0, pk_c]]
          )
         (script [
           ver [arg 0, pk_c],
           ver [arg 1, pk_p]])]
-}
s = var "s"
--k is security parameter, n is number of people
inS' k n = 
 script [
--take 1 input
  inputs 1,
--calculate the size
  size [arg 0],
--set it equal to s
  set "s",
--is s in the interval [8k, 8(k+n-1)]?
  within [s, con (8*k), con (8*(k+n-1)+1)],
--is s mod 8 = 0?
  (s .% con 8) .= con 0]

--"compile" the function inSk' so that it can be used in other scripts.
--inS k n = fun (inS' k n)
inS k n = makeFun "inSkN" False
 
--winner choosing function  
f' k n = 
 script [
--n inputs representing the strings chosen by n players
  inputs n,
--sum [arg 0, arg 1,..., arg (n-1)] mod n + 1 is the winning player.
  ((sums (map arg [0..(n-1)])) .% (con n) .+ con 1)]

f:: Int -> Int -> Function Int
--f k n = fun (f' k n)
f k n = makeFun "fkn" False

sA = var "sA"
sB = var "sB"
s1 = var "s1"
s2 = var "s2"
body = var "body"

compute:: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Circuit2 Int
compute k n a b a' b' h_A h_B = 
 let 
   inSkN = inS k n
   fkn = f k n
 in
  script [
   inputvars ["body", "s1", "s2", "sA", "sB"],
--sA,sB are in SkN, and the hash of sA and sB are hA and hB
   ((inSkN [sA] .& inSkN [sB] .& (hash [sA] .= con h_A) .& (hash [sB] .= con h_B)) .&
--and EITHER the winner is a and the signature matches a's, or the winner is b and the signature matches b's, or we have both signatures
    ((fkn [sA, sB] .= con a) .& ((std_trans_f a') [body, s1]) .|
     (fkn [sA, sB] .= con b) .& ((std_trans_f b') [body, s2]))) .|
    ((std_trans_f a') [s1] .& (std_trans_f b') [s2])]

testC::Circuit2 Int -> IO ()
testC c = do
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

main = (testC (inS' 3 2)) >> (testC (compute 3 2 30 40 50 60 70 80))
