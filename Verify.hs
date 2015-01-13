module Verify where

import Data.List
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

--to mark a transaction as provably unspendable is with a scriptPubKey of the following form: 
-- scriptPubKey: OP_RETURN {zero or more ops}
--unspendable' :: (Empty) -> Circuit2 Int
unspendable' = 
	script [
	 inputs 2,
	 ret[arg 0, arg 1]]

--To spend the transaction, need to come up with some data such that hashing the data twice results in the given hash. 
--p is the given hash puzzle
puzzle' p = 
	script [
	 inputs 1,
	 con p .= hash256 [arg 0]]


--Standard script sending money to a script instead of a Bitcoin address (P2SH = Pay-To-Script (BIP 16)). The script must be one of the other standard output scripts.
--scriptPubKey: OP_HASH160 [hashOfScript] OP_EQUAL
--scriptSig: [signatures as required by script][script]
pay_to_script' h = 
	script [
	 inputs 2,
	 con h .= hash [arg 1]]


class PrintAllType t where
    printAll' :: [String] -> t
 
instance PrintAllType (IO a) where
    printAll' acc = do mapM_ putStrLn acc
                       return undefined
 
instance (Show a, PrintAllType r) => PrintAllType (a -> r) where
    printAll' acc = \x -> printAll' (acc ++ [show x])
 
printAll :: (PrintAllType t) => t
printAll = printAll' []



