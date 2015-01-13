module Test1 where

import Data.List
import Data.Maybe
import Circuit
import ScriptDefs
import Compiler

multisig' m n li = 
	script [
	 inputs m,
	 multiver $ (map arg [0..m-1]) ++ [con m] ++ (map con li)++ [con n]]
