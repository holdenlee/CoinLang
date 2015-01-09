{-# OPTIONS
 
 -XRankNTypes
#-}

module Compiler where

import Data.List
import Data.Maybe
import Circuit
import Data.Bimap
import Utilities

byList :: (forall a. (a -> d) -> [a] -> [a]) -> ((a -> d) -> ([a], [b]) -> ([a],[b]))
byList f (as,bs) g = unzip $ f (\(x,y) -> g x) (zip as bs)

byList2 :: (forall a. (a -> d) -> [a] -> a) -> ((a -> d) -> ([a], [b]) -> (a,b))
byList2 f (as,bs) g = f (\(x,y) -> g x) (zip as bs)

revneg :: [Int] -> [Int]
revneg = reverse $ map (\x -> -x)

isSym :: Gate a -> Bool
isSym g = case g of 
               Fun f True args -> True
               _ -> False

sortIfSym :: Gate a -> [Int] -> [Int]
sortIfSym g xs = if isSym g then sort xs else xs

--data StackPos = OnStack (Bool, Int, Int) | IsConst (deriving Eq)
--instance Comparable StackPos where
--    compare s1 s2 = 

type BI = Bimap Int Int

indexToStackPos :: BI -> Int -> Int
indexToStackPos b i = removeJust $ Data.Bimap.lookup i b

compile:: Circuit Int -> String
compile circ = let
    --replace each Fun "f" [x,y,...] with Fun "f" [Arg x0, Arg y0,...]
    circ2 = inTermsOfArgs circ
    --the number of args is the largest initial sublist all in the form Arg x
    numArgs = length (takeWhile (\x -> case x of Arg y -> True
                                                 _ -> False) circ2)
    bimap::Bimap Int Int
    bimap = empty
    inList::[[Int]]
    --inList!!j is a list of all gates leading to j
    inList = map (\x -> case x of Fun s li -> map removeArg li
                                  _ -> []
                 ) circ2
    --outList!!j is a list of all gates leading from j
    outList = map (\i -> findIndices (\j -> i `elem` (inList!!j)) [0..(length circ2 - 1)]) [0..(length circ2 - 1)]
    (prog, _, _, _) = loopUntil
                 (\(_,_,_,z) -> and z) --all z are True
                 (\(str, b, top, dones) ->
                  let
                      --the indices of gates not evaluated yet
                      unevalInds = findIndices not dones
                      --the input lists of those gates
                      unevalIns = zipWith (!!) inList unevalInds
                      filterOutConsts::[Int] -> Maybe [Int]
                      filterOutConsts ins = 
                          sequence $ do
                            x <- ins
                            y <- if (dones!!x) 
                                 then [x]
                                 -- if x has already been processed, return x.
                                      else
                                          case circ2!!x of
                                            --if it refers to a constant, we're OK.
                                            Const y -> []
                                            --else we can't include this gate yet.
                                            _ -> [Nothing]
                            return y
                      --retain only those indices and corresponding input lists that have all arguments either evaluated already, or constants.
                      (unevalIns2, unevalInds2) = (byList (Data.List.filter isJust)) (map filterOutConsts unevalIns, unevalInds)
                      unevalIns3 = map removeJust unevalIns2
                      --if the program runs correctly there should be no problem with the removeJust
                      --look up the stack positions of those indices
                      stackPoss = map (removeJust.(flip Data.Bimap.lookup b)) unevalIns3
                      stackPoss2 = map (\(i,ss) -> if isSym circ2!!i then sort ss else ss) $ zip unevalInds2 stackPoss
                      (bestInd, bestSP) = (mapSnd revneg) (byList2 minimum) ((map revneg) stackPoss2, unevalInds2)
                      --this is the index of the next guy, and the stack positions of its arguments
                      --origB::Bimap Int Int
                      --origB = fromList $ zip unevalInds unevalIns
                      --origIns = removeJust $ lookup bestInd origB
                      inps = inList!!bestInd
                      constInds = inps \\ unevalIns3
                      --(str2,b2,top2,dones2) = foldIterate (addConstToStack circ2) constInds ("",b,top,dones)
                      willAppears = map (willBeUsedAgain outList dones) inps
                      topB = case (circ2!!bestInd) of
                               --symmetric case
                               Fun _ True _ ->
                                   stopBefore (\i -> i<0 || (indexToStackPos b i)) (\x -> x-1)  0
                               --not symmetric case
                               _ -> 
                                   if loopUntil (\(expected,i) -> (not dones!!(inps!!i))|| (i/=0 && (circ2!!(inps!!i) == expected))) (\(expected,i) -> (indexToStackPos b (inps!!i) + 1,i+1) (0,0)) == top + 1 then indexToStackPos b 0 else top + 1
                      (b2,inps2) = --(str2, b2, top2, dones2) = 
                           case (circ2!!bestInd) of
                            Fun _ True _ -> 
                                foldIterate (\i (b1,i1) -> (Data.Bimap.delete i b1,Data.List.delete i i1)) (map (\i -> removeJust.(lookupR i b)) [topB..top]) (b,inps)
--make them done?
                            _ -> (b,inps)
                      --should do something like this before.
                      (str3, b3, btemp3, top3, dones3) = foldIterate (process circ2 outList) inps (str, b, b2, top, dones)
                      b4 = b3 |> Data.Bimap.insert bestInd (top3 + 1)
                      -- warning: this doesn't take it off.
                      top4 = top3 + 1
                      dones4 = listUpdate bestInd True dones3
                      str4 = case circ2!!bestInd of
                               Fun f _ _ -> str3 ++ f
                               _ -> str3
                  in (str4, b4, top4, dones4)) ("", bimap, 0, replicate (length circ2) False)
  in prog

willBeUsedAgain :: [[Int]] -> [Bool] -> Int -> Bool
willBeUsedAgain outs dones i =
    (length (Data.List.filter (\j -> not (dones!!j)) (outs!!i)) > 1)    

--permanent BI, temporary BI
process :: Circuit Int -> [[Int]] -> Int -> (String, BI, BI, Int, [Int]) -> (String, BI, BI, Int, [Int])
process circ outs i (str, b, btemp, top, dones) =
    let
        --stack position
        sp = indexToStackPos circ i
        d = dones!!i
        dones2 = listUpdate i True dones
    in
        case circ!!i of
          Const x -> 
              let 
                  str2 = str ++ (show x) ++ " "
                  btemp2 = btemp |> Data.Bimap.insert i (top + 1)
                  top2= top + 1
              in
                (str2, b, btemp2, top, dones2)
          _ -> 
             if willBeUsedAgain outs dones i
                then 
                    let
                        str2 = str ++ "OP_ROLL " ++ show (top - sp + 1) ++ " "
                        btemp2 = btemp |> Data.Bimap.insert i (top + 1)
                    in (str2, b, btemp2, top, dones2)
     --if done, then move (with consequences)
                else
                    let
                        str2 = str ++ "OP_PICK " ++ show (top - sp + 1) ++ " "
                        b2 = b |> Data.Bimap.delete i |> bmapR (\x -> if x>i then x-1 else x)
                        btemp2 = btemp |> bmapR (\x -> iflist [(x>i, x-1),
                                                              (x==i, top),
                                                              (x<i, x)])
                        top2 = top + 1
                    in (str2, b2, btemp2, top2, dones2)
        

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

