{-# OPTIONS
 
 -XNoMonomorphismRestriction
#-}

module ScriptDefs where

import Data.List
import Data.Maybe
import Circuit

{-Syntax: make a function by: 

makeFun "FUNCTION_NAME" isSym

where isSym is True if order of arguments doesn't matter, and False if it does matter.

Arguments must be given in list form. To use as infix, define an operator 

(<symbol>) x y = f [x,y]
-}

infixr 2 .|
infixr 3 .&
infix 4 .=
infixl 6 .+
infixl 7 .%

ret = makeFun "OP_RETURN" False
eq = makeFun "OP_EQUAL" True
(.=) x y = eq [x,y]
hash = makeFun "OP_HASH160" False
h x = hash [x]
hash256 = makeFun "OP_HASH256" False
ver = makeFun "OP_CHECKSIG" False
size = makeFun "OP_SIZE OP_NIP" False
--note OP_SIZE does not pop, so OP_NIP is for consistency.
lt = makeFun "OP_LESSTHAN" False
gt = makeFun "OP_GREATERTHAN" False
le = makeFun "OP_LESSTHANOREQUAL" False
(.<=) x y = le [x,y]
ge = makeFun "OP_GREATERTHANOREQUAL" False
(.>=) x y = ge [x,y]
within = makeFun "OP_WITHIN" False
md = makeFun "OP_MOD" False
(.%) x y = md [x,y]
add = makeFun "OP_ADD" True
(.+) x y = add [x,y]
sums li = 
    let 
        n = length li
    in
      makeFun (cutLast $ concat $ replicate (n-1) "OP_ADD ") True li
--alternative: is easier
--bands = foldr1 (.+)

band = makeFun "OP_BOOLAND" True
(.&) x y = band [x,y]
bands li = 
    let 
        n = length li
    in
      makeFun (cutLast $ concat $ replicate (n-1) "OP_BOOLAND ") True li
--alternative: is easier
--bands = foldr1 (.&)
bor = makeFun "OP_BOOLOR" True
(.|) x y = bor [x,y]
bors li = 
    let 
        n = length li
    in
      makeFun (cutLast $ concat $ replicate (n-1) "OP_BOOLOR ") True li
--alternative: is easier
--bors = foldr1 (.|)

ret = makeFun "OP_RETURN" True
multiver = makeFun "OP_CHECKMULTISIG" False

cutLast::[a] -> [a]
cutLast li = reverse $ tail $ reverse li
