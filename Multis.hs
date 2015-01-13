module Multis where

import Data.List
import Data.Maybe
import Circuit
import ScriptDefs
import Compiler


--where 1 <= n <= 3 and 1 <= m <= n are integers representing that m out of n signatures need to appear to claim the transaction.
multisig' m n li = 
	script [
	 inputs m,
	 multiver $ (map arg [0..m-1]) ++ [con m] ++ (map con li)++ [con n]]
