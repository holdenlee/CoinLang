{-# OPTIONS
 
 -XNoMonomorphismRestriction
#-}

module ScriptDefs where

import Data.List
import Data.Maybe
import Circuit

eq = makeFun "OP_EQUAL" True
hash = makeFun "OP_HASH160" False
ver = makeFun "OP_CHECKSIG" False

