module MultipartyLottery where

import Data.List
import Data.Maybe
import Circuit
import ScriptDefs
import Compiler
import CircuitViz
import Utilities

import Verify

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
--is s in the interval [8k, 8(k+n-1)]? (Note constants must be marked with 'con'
  within [s, con (8*k), con (8*(k+n-1)+1)],
--is s mod 8 = 0?
  (s .% con 8) .= con 0]

--"compile" the function inSk' so that it can be used in other scripts.
inS k n = fun (inS' k n)
--inS k n = makeFun "inSkN" False
 
--winner choosing function  
winner' k n = 
 script [
--n inputs representing the strings chosen by n players
  inputs n,
--sum [arg 0, arg 1,..., arg (n-1)] mod n is the winning player.
  (sums (map arg [0..(n-1)])) .% (con n)]

winner k n = fun (winner' k n)
--winner k n = makeFun "fkn" False

sA = var "sA"
sB = var "sB"
s1 = var "s1"
s2 = var "s2"
body = var "body"

--the 2-party version with stronger security
compute2' k pk_a pk_b h_A h_B = 
 let 
   inSkN = inS k 2
   fkn = winner k 2
 in
  script [
   inputvars ["s1", "s2", "sA", "sB"],
--sA,sB are in SkN, and the hash of sA and sB are hA and hB
   ((inSkN [sA] .& inSkN [sB] .& (hash256 [sA] .= con h_A) .& (hash256 [sB] .= con h_B)) .&
--and EITHER the winner is a and the signature matches a's, or the winner is b and the signature matches b's, or we have both signatures
    ((fkn [sA, sB] .= con 0) .& ((simple_ver pk_a) [s1]) .|
     (fkn [sA, sB] .= con 1) .& ((simple_ver pk_b) [s2]))) .|
    ((simple_ver pk_a) [s1] .& (simple_ver pk_b) [s2])]

--PutMoney^B in the secure 2-party lottery
putMoneyB' k pk_B h_B = 
 script [
  inputvars ["s2", "sB"],
  (simple_ver pk_B) [s2],
  (inS k 2) [sB],
  hash256 [sB] .= con h_B]

computeN' k pks hs = 
    let 
        n = length pks
        --arg 0 is the signature
        sig = arg 0
        --[arg 1,...,arg n] are the s_i
        ss = map arg [1..n]
    in
      script [
        inputs (n+1),
        --band means "boolean and" all of these
        --are s_1,...,s_N \in S_k^N?
        band $ map (\s_i -> inS k n [s_i]) ss,
        --are H(s_i)=h_i for all i?
        band $ map (\(s_i, h_i) -> hash256 [s_i] .= con h_i) (zip ss hs),
        --for some i, is f(s_1,...,s_N) = i (the winner is i) and ver_i(sig) (the winner signs)?
        bor $ map (\i -> ((winner k n) ss .= con i) .& (simple_ver (pks!!i)) [sig]) [0..(n-1)]]
