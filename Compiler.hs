module Compiler where

import Data.List
import Data.Maybe
import Circuit
import Data.Bimap

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

ifelselist:: [(Bool, a)] -> a -> a
ifelselist li y = 
  case li of 
    [] -> y
    ((b,x):li2) -> if b then x else ifelselist li2 y

iflist :: [(Bool, a)] -> a
iflist li = 
  case li of 
    ((b,x):li2) -> if b then x else iflist li2

zipSort :: (b -> b -> Ordering) -> [a] -> [b] -> ([a],[b])
zipSort as bs f =
  unzip (sortBy (\(x1,y1) (x2,y2)-> f y1 y2) (zip as bs))

--lexicographic ordering
lexCompare:: (Ord a) => [a] -> [a] -> Ordering
lexCompare xs ys = 
  let
--yay lazy evaluation
    x = head x
    x2= tail x
    y = head y
    y2= tail y
  in
    iflist [((xs==[] && ys==[]),EQ),
            (xs==[], LT),
            (ys==[], GT),
            (x<y, LT),
            (x>y, GT),
            (x==y, lexCompare x2 y2)]

--Bimap maps the element at the given stack position to the top
--Int is top of stack
--roll::Int -> (Circuit a,Bimap Int Int,Int) -> (Circuit a,Bimap Int Int,Int)
--roll i (c,inds,top) = 

--write me

bmapR:: (b -> b) -> Bimap a b -> Bimap a b
bmapR f bmap = fromList $ map (\(x,y) -> (x,f y)) (toList bmap)

pick::Int -> (Circuit a,Bimap Int Int,Int) -> (Bimap Int Int,Int,String)
pick i (c,bmap,top) = 
  let 
    rot = \x -> if x==i then top else (if x>i then i-1 else x)
    bmap2 = bmapR rot bmap
  in
    (bmap2,top+1, "OP_PICK "++(show (top-i)))
--or is it top-i+1?


--
nextEval::Circuit a -> [Int] -> [Bool] -> Bool -> [Int] -> ([Int], String)
nextEval c inds appAgains isSym inps = ([],"")
--also include: symmetric or not
--example: 321 TTT will roll all 3
--321 FFF will use up all 3
--12 FT will roll 2 to top  

