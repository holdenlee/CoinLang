module ScriptDefs where

import Data.List
import Data.Maybe
import Circuit

eq = makeFun "OP_EQUAL"
hash = makeFun "OP_HASH160"
ver = makeFun "OP_CHECKSIG"

