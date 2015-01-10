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

eq = makeFun "OP_EQUAL" True
(.=) x y = eq [x,y]
hash = makeFun "OP_HASH160" False
h x = hash [x]
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
sums li = 
    let 
        n = length li
    in
      makeFun (cutLast $ concat $ replicate (n-1) "OP_SUM ") True li
add = makeFun "OP_SUM" True
(.+) x y = add [x,y]

band = makeFun "OP_BOOLAND" True
(.&) x y = band [x,y]
bor = makeFun "OP_BOOLOR" True
(.|) x y = bor [x,y]

cutLast::[a] -> [a]
cutLast li = reverse $ tail $ reverse li
