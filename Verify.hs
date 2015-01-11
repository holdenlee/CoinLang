module Verify where

import Data.List
import Data.Maybe
import Circuit
import ScriptDefs
import Compiler

--A standard transaction
--arg 0 is the signature
--arg 1 is the public key
--pkh is the public key hash
verify'::Int -> Circuit2 Int
verify' pkh =
 script [
  inputs 2,
  ver [arg 0, arg 1],
  con pkh .= hash [arg 1]]

verify x = fun $ verify' x

simple_ver' :: Int -> Circuit2 Int
simple_ver' pk = 
 script [
  inputs 1,
  ver [arg 0, con pk]]

simple_ver x = fun $ simple_ver' x
