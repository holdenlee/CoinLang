module Main where

import Compiler
import Verify
import MultipartyLottery
import Multis

main = do
  let a = verify' 999
  let b = compute2' 9 11 22 111 222
  let c = computeN' 9 [11,22,33] [111,222,333]
  let d = multisig' 2 3 [11, 22, 33]
  let e = unspendable'
  let f = puzzle' 999
  let g = pay_to_script' 333
  let h = simple_ver' 111
  writeFile "sample_scripts/verify.txt" (compile a)
  writeFile "sample_scripts/compute2.txt" (compile b)
  writeFile "sample_scripts/compute3.txt" (compile c)
  writeFile "sample_scripts/verifyC.txt" (toHex $ compile a)
  writeFile "sample_scripts/compute2C.txt" (toHex $ compile b)
  writeFile "sample_scripts/compute3C.txt" (toHex $ compile c)
  writeFile "sample_scripts/multisig.txt" (compile d)  
  writeFile "sample_scripts/multisigC.txt" (toHex $ compile d)    
  writeFile "sample_scripts/unspendable.txt" (compile e)
  writeFile "sample_scripts/unspendableC.txt" (toHex $ compile e)
  writeFile "sample_scripts/puzzle.txt" (compile f)
  writeFile "sample_scripts/puzzleC.txt" (toHex $ compile f)
  writeFile "sample_scripts/p2sh.txt" (compile g)
  writeFile "sample_scripts/p2shC.txt" (toHex $ compile g)  
  writeFile "sample_scripts/obsolete.txt" (compile h)
  writeFile "sample_scripts/obsoleteC.txt" (toHex $ compile h)
  
  
