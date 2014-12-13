module Main where

import Data.List
import Data.Maybe

data Gate a = Const a | Arg Int | Fun String [Gate a]

instance (Eq a) => Eq (Gate a) where
  Const x == Const y = x == y
  Arg x == Arg y = x == y
  Fun s li == Fun s' li' = (s==s') && (li == li')
  _ == _ = False

instance (Show a) => Show (Gate a) where
  show (Const x) = "Const " ++ show x
  show (Arg x) = "Arg " ++ show x
  show (Fun str gs) = str ++ "(" ++ (intercalate "," (map show gs)) ++ ")"

type Circuit a = [Gate a]

changeAt :: Int -> a -> [a] -> [a]
changeAt i x li = (take i li)++[x]++(drop (i+1) li)

replaceArgs' :: (Eq a) => Circuit a -> Gate a -> Gate a
replaceArgs' c g = case g of
                    Const x -> Const x
                    Arg i -> Arg i 
                    Fun s li -> Fun s (map (replaceArgs' c) li)
                                --(map (replaceArgs' c) li)

replaceArgs :: (Eq a) => Circuit a -> Circuit a
replaceArgs circ = foldl
                   (\c i -> changeAt i (replaceArgs' c (c!!i)) c) circ
                   [0..(length circ - 1)]
--map (replaceArgs' circ) circ
--there's a subtle reason this doesn't work!

inTermsOfArgs' :: (Eq a) => Circuit a -> Gate a -> Gate a
inTermsOfArgs' circ g = case g of
                         Const x -> Const x
                         Arg i -> Arg i
                         Fun s li -> 
                           Fun s (map (\gate -> Arg (removeJust (elemIndex gate circ))) li)
                           --Arg (removeJust (elemIndex (Const x) circ))

inTermsOfArgs :: (Eq a) => Circuit a -> Circuit a
inTermsOfArgs c = map (inTermsOfArgs' c) c  

makeFun :: (Eq a) => String -> [Circuit a] -> Circuit a
makeFun name gates =
  let
    (unioned, args) = foldl (\(u, as) li ->
                                 let
                                   u2 = union u li
                                     in (u2, as++[(last li)])) ([],[]) gates -- (tail li) u2
                      --Arg (removeJust (elemIndex (last li) u2))
  in
   unioned ++ [Fun name args]

--in (u2, as++[last li])) ([],[]) gates

(|>) :: a -> (a -> b) -> b
x |> f = f x

(.&) :: (Eq a) => Circuit a -> Circuit a -> Circuit a
circ .& more = (union circ (more))
--replaceArgs 

eq = makeFun "OP_EQUAL"
hash = makeFun "OP_HASH160"
ver = makeFun "OP_CHECKSIG"

inputs:: Int -> Circuit Int
inputs i = map Arg [0..(i-1)]

std_trans::Int -> Circuit Int
std_trans pkh =
  inputs 2 .&
  ver [arg 0, arg 1] .&
  eq [con pkh, hash [arg 1]]
--add aliases so don't need the brackets inside

arg x = [Arg x]

con x = [Const x]

--now need to compile script...

removeJust:: Maybe a -> a
removeJust x = case x of Just y -> y

removeArg:: Gate Int -> Int
removeArg circ = case circ of Arg z -> z

compile:: Circuit Int -> String
compile circ = let
    --replace each Fun "f" [x,y,...] with Fun "f" [Arg x0, Arg y0,...]
    circ2 = inTermsOfArgs circ
    --the number of args is the largest initial sublist all in the form Arg x
    numArgs = length (takeWhile (\x -> case x of Arg y -> True
                                                 _ -> False) circ2)
    indexOfGates = [0..(length circ2 - 1)]
    inList::[[Int]]
    --inList!!j is a list of all gates leading to j
    inList = map (\x -> case x of Fun s li -> map removeArg li
                                  _ -> []
                 ) circ2
    --outList!!j is a list of all gates leading from j
    outList = map (\i -> findIndices (\j -> i `elem` (inList!!j)) [0..(length circ2 - 1)]) [0..(length circ2 - 1)]
    (prog, is) = foldl 
                      --at index i
                      --indexs!!j is the position of gate j in the stack.
                       (\(str,indexs) i ->
                        --if it's a function
                        case circ2!!i of
                         Fun name args ->
                          let
                            --true if this argument is not an input for any later ones
                            appearsAgain = not (all (\x -> not (i `elem` x)) (drop (i+1) inList))
                            --if this does not appear again, then we will remove it from the stack immediately afterwards, so do not increase the stack index. If it does, then the stack index will be 1 more than the previous. (Might change this behavior...)
                            indices2 = changeAt i (if appearsAgain then (indexs!!(i-1) + 1) else (indexs!!(i-1))) indexs
                            --consider the 0th argument of the f of the given gate. If it's Arg x, how far is it back in the stack? It's indexs!!(i-1) + 1 - indexs!!x. Now add j for the jth argument because we're rolling things on top of the stack.
                            picknums = map (\(j,x) -> (indexs!!(i-1)) + j + 1 - (indices2!!x)) (zip [0..] (inList!!i))
                            -- zip with j?
                          in
                           (str ++ concat (map (\x -> " " ++ show x ++ " OP_PICK") picknums) ++ " " ++ name ++ (if appearsAgain || (i==length circ2 - 1) then "" else " OP_VERIFY"), indices2)
                         Const y -> (str ++ " " ++ show y, changeAt i (indexs!!(i-1) + 1) indexs)
                        ) ("", indexOfGates) [numArgs..(length circ2 - 1)]
  in
     prog
-- ++ show inList ++ show outList ++ show is
                        

main::IO ()
main = do
  putStrLn (show (std_trans 999))
  putStrLn (show $ inTermsOfArgs (std_trans 999))
  putStrLn (compile (std_trans 999))
  
