module Circuit where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Utilities

data Gate a = Const a | Arg Int | Fun String Bool [Gate a] | Var String
--Fun f isSymmetric inputGates

type Circuit a = [Gate a]
type Circuit2 a = ([Gate a], M.Map String Int)
type Function a = [Circuit2 a] -> Circuit2 a

--(!>)::Circuit2 a -> Int -> Gate a
--c!>i = (fst c)!>i

instance (Eq a) => Eq (Gate a) where
  Const x == Const y = x == y
  Arg x == Arg y = x == y
  Fun s _ li == Fun s' _ li' = (s==s') && (li == li')
  _ == _ = False

instance (Show a) => Show (Gate a) where
  show (Const x) = "Const " ++ show x
  show (Arg x) = "Arg " ++ show x
  show (Fun str _ gs) = str ++ "(" ++ (intercalate "," (map show gs)) ++ ")"
  show (Var x) = "Var " ++ x

changeAt :: Int -> a -> [a] -> [a]
changeAt i x li = (take i li)++[x]++(drop (i+1) li)

--Bool: is it the 1st time?
replaceArgs' :: (Eq a) => Bool -> Circuit2 a -> Gate a -> Gate a
replaceArgs' b (c,m) g = case g of
                    Const x -> Const x
                    Arg i -> Arg i 
                    Fun s b li -> Fun s b (map (replaceArgs' False (c,m)) li)
                    Var s -> if b then Var s else c!!(lookup2 s m)
--c!!(lookup2 s m)
--Arg (lookup2 s m)
--                    Var s -> c!!(lookup2 s m)
                                --(map (replaceArgs' c) li)

replaceArgs :: (Eq a) => Circuit2 a -> [Gate a]
replaceArgs (circ,m) = foldl
                   (\c i -> changeAt i (replaceArgs' True (c,m) (c!!i)) c)
                   circ
                   [0..(length circ - 1)]
--map (replaceArgs' circ) circ
--there's a subtle reason this doesn't work!

inTermsOfArgs' :: (Eq a) => Circuit a -> Gate a -> Gate a
inTermsOfArgs' circ g = case g of
                         Const x -> Const x
                         Arg i -> Arg i
                         Fun s isSym li -> 
                           Fun s isSym (map (\gate -> Arg (removeJust (elemIndex gate circ))) li)
                         Var s -> Var s
                           --Arg (removeJust (elemIndex (Const x) circ))

inTermsOfArgs :: (Eq a) => [Gate a] -> [Gate a]
inTermsOfArgs c = map (inTermsOfArgs' c) c  

makeFun :: (Eq a) => String -> Bool -> [Circuit2 a] -> Circuit2 a
makeFun name isSym gates =
  let
    (unioned, args) = foldl (\(u, as) (li,_) ->
                                 let
                                   u2 = union u li
                                     in (u2, as++[(last li)])) ([],[]) gates -- (tail li) u2
                      --Arg (removeJust (elemIndex (last li) u2))
  in
   (unioned ++ [Fun name isSym args],M.empty)

set :: String -> Circuit2 a
set var = ([], M.singleton var (-1))

--in (u2, as++[last li])) ([],[]) gates

(.>) :: (Eq a) => Circuit2 a -> Circuit2 a -> Circuit2 a
(circ,m1) .> (more,m2) = ((union circ (more)), m1 `M.union` (M.map (+(length circ)) m2))
--replaceArgs 

script :: (Eq a) => [Circuit2 a] -> Circuit2 a
script = foldl1 (.>)

inputs:: Int -> Circuit2 Int
inputs i = (map Arg [0..(i-1)], M.empty)

inputvars:: [String] -> Circuit2 Int
inputvars li = let 
    l = length li - 1
 in
    (map Arg [0..l], M.fromList $ zip li [0..l])

--add aliases so don't need the brackets inside

arg x = ([Arg x], M.empty)

con x = ([Const x], M.empty)

var x = ([Var x], M.empty)

--now need to compile script...
removeArg:: Gate Int -> Int
removeArg circ = case circ of Arg z -> z




                        


  
