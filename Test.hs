{-# OPTIONS
 
 -XRankNTypes
#-}

module Compiler where

import Data.List
import Data.Maybe

f:: (forall a. a-> a) -> (a,b) -> (a,b)
f g (x,y) = (g x, g y)
