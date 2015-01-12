module Main where

import Compiler
import Verify
import MultipartyLottery

main = do
  let a = verify' 999
  let b = compute2' 9 11 22 111 222
  let c = computeN' 9 [11,22,33] [111,222,333]
  writeFile "sample_scripts/verify.txt" (compile a)
  writeFile "sample_scripts/compute2.txt" (compile b)
  writeFile "sample_scripts/compute3.txt" (compile c)
  writeFile "sample_scripts/verifyC.txt" (toHex $ compile a)
  writeFile "sample_scripts/compute2C.txt" (toHex $ compile b)
  writeFile "sample_scripts/compute3C.txt" (toHex $ compile c)
