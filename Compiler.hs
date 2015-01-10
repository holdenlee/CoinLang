{-# OPTIONS
 
 -XRankNTypes
#-}

module Compiler where

import Data.List
import Data.Maybe
import Circuit
import Data.Bimap
import Utilities
import Debug.Trace -- temporary
import Data.List.Utils

byList :: (forall a. (a -> d) -> [a] -> [a]) -> ((a -> d) -> ([a], [b]) -> ([a],[b]))
byList f g (as,bs) = unzip $ f (\(x,y) -> g x) (zip as bs)

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

--byList2 :: (forall a. (a -> a -> d) -> [a] -> a) -> ((a -> a ->d) -> ([a], [b]) -> (a,b))
--byList2 f g (as,bs) = f (\(x,y) (x1,y1)-> g x) (zip as bs)

revneg :: [Int] -> [Int]
revneg li = reverse $ map (\x -> -x) li

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
indexToStackPos b i = removeJust $ Data.Bimap.lookup i (b `debug` ("(b,i):" ++ show (b,i)))

compile:: Circuit2 Int -> String
compile circ = let
    --replace each Fun "f" [x,y,...] with Fun "f" [Arg x0, Arg y0,...]
    circ2 = inTermsOfArgs (replaceArgs circ)
    --the number of args is the largest initial sublist all in the form Arg x
    numArgs = length (takeWhile (\x -> case x of Arg y -> True
                                                 _ -> False) circ2) `debug` ("circ2: "++show circ2)
    --ignore the variables
    dones0 = map (\(i,g) -> case g of 
                              Arg _ -> True
                              Var _ -> True
                              _ -> False
                 ) 
             (zip [0..] circ2)
    bimap::Bimap Int Int
    bimap = Data.Bimap.fromList $ map (\x-> (x,x)) [0..(numArgs - 1)] `debug` ("dones0: " ++ show dones0)
    inList::[[Int]]
    --inList!!j is a list of all gates leading to j
    inList' = map (\x -> case x of Fun s _ li -> map removeArg li
                                   _ -> []
                 ) circ2
    inList = inList' `debug` ("in: " ++ show inList')
    --outList!!j is a list of all gates leading from j
    outList' = map (\i -> findIndices (\j -> i `elem` (inList!!j)) [0..(length circ2 - 1)]) [0..(length circ2 - 1)] 
    outList = outList' `debug` ("\nout: " ++ show outList')
    (prog, _, _, _) = loopUntil
                      (\(x1,x2,x3,z) -> --trace ("step \n" ++ show (x1,x2,x3)) $ 
                                        and z) --all z are True
                      (stepCompile circ2 inList outList)
                      ("", bimap, numArgs - 1, dones0)
  in prog |> postProcess



stepCompile :: Circuit Int -> [[Int]] -> [[Int]] -> (String,BI, Int, [Bool]) -> (String,BI, Int, [Bool]) 
stepCompile circ2 inList outList (str, b, top, dones) =
                  let
                      --the indices of gates not evaluated yet
                      unevalInds = (findIndices not dones) `debug` ("inputs:" ++ (show (str, b, top, dones))) 
                      --the input lists of those gates
                      unevalIns = map (\i -> inList!!i) unevalInds `debug` ("unevalInds" ++ show unevalInds)
                      filterOutConsts::[Int] -> Maybe [Int]
                      filterOutConsts ins = 
                          (fmap concat $ sequence $ map (\z -> if (dones!!z) 
                                 then Just [z]
                                 -- if x has already been processed, return x.
                                      else
                                          case circ2!!z of
                                            --if it refers to a constant, we're OK.
                                            Const _ -> Just []
                                            --else we can't include this gate yet.
                                            _ -> Nothing) 
                          ins) |> (\y -> case y of 
                                           Just [] -> Nothing
                                           _ -> y)
                      --retain only those indices and corresponding input lists that have all arguments either evaluated already, or constants.
                      (unevalIns2, unevalInds2) = (byList Data.List.filter isJust) (map filterOutConsts unevalIns, unevalInds)
                      unevalIns3 = map removeJust unevalIns2
                      --if the program runs correctly there should be no problem with the removeJust
                      --look up the stack positions of those indices
                      --does not take into account the savings you get from things at the top that will not be used any longer...
                      stackPoss = map2d (removeJust.(flip Data.Bimap.lookup b)) (unevalIns3 `debug` ("unevalIns3" ++ show unevalIns3))
                      stackPoss2 = map (\(i,ss) -> if isSym (circ2!!i) then sort ss else ss) $ zip unevalInds2 stackPoss `debug` ("sp: "++ show stackPoss)
                      (bestSP, (bestIns,bestInd)) = (mapFst revneg) $ minimum $ zip ((map revneg) stackPoss2) $ zip unevalIns3 unevalInds2
                      --this is the index of the next guy, and the stack positions of its arguments
                      --origB::Bimap Int Int
                      --origB = fromList $ zip unevalInds unevalIns
                      --origIns = removeJust $ lookup bestInd origB
                      inps = inList!!bestInd `debug` ("best: " ++ show (bestSP, (bestIns,bestInd)))
                      constInds = inps \\ bestIns
                      --(str2,b2,top2,dones2) = foldIterate (addConstToStack circ2) constInds ("",b,top,dones)
                      dones2 = listUpdate bestInd True dones
                      willAppears' = map (willBeUsedAgain outList dones2) [0..(length circ2 - 1)]
                      willAppears = willAppears' `debug` ("willAppears: " ++ show (willAppears'))
--note: inps contains the constants. bestIns does not.
                      topB' = case (circ2!!bestInd `debug` ("inps: " ++ (show inps))) of
                               --symmetric case
                               Fun _ True _ ->
                                   loopUntil (\i -> i<0 || not (i `elem` (map (indexToStackPos b) bestIns))) (\i -> i-1) top
--keep subtracting 1 from i until i<0 or there is no input at stack position i.
                               --not symmetric case
                               _ -> 
                                   if fst (loopUntil 
                                          (\(expected,i) -> i>=(length inps) `debug` (show i ++ " " ++ show b) || (willAppears!!(inps!!i))|| (i/=0 && ((Data.Bimap.lookup (inps!!i) b) /= Just expected)))
--loop until: we've exhaused all inputs, or we find something that will appear again, or we're not on the 0th index and we find something that's not one more than previous stack position (the expected)
                                          (\(expected,i) -> ((indexToStackPos b (inps!!i)) + 1,i + 1)) 
                                          (0,0))
                                          == top + 1 
                                    then indexToStackPos b (inps!!0) 
                                    else top + 1
                      topB = topB' `debug` ("topB: "++ (show topB'))
                      (b2,inps2) = --(str2, b2, top2, dones2) = 
                           case (circ2!!bestInd) of
--symmetric case
                            Fun _ True _ -> 
                                foldl 
                                (\(b1,i1) i -> (Data.Bimap.delete i b1,Data.List.delete i i1)) 
                                (b,inps) -- this contains the constants at the end
                                (map (\i -> removeJust (lookupR i b)) [(topB+1)..top])
--delete the data at stack positions (topB+1) to (top), and delete it from the inputs
--make them done?
                                 `debug` "D1"
--nonsymmetric case
                            _ ->
                                if topB > top 
                                   then (b, inps)
                                   else (b, drop (top - topB + 1) inps)
                      --should do something like this before.
                      (str3, b3, btemp3, top3, dones3) = foldIterate (process circ2 inList outList bestInd) inps2 (str, b, b2, top, dones2) `debug` ("after removed: "++ show inps2) -- this needs to be inps2
                  in
                   case (circ2!!bestInd `debug` ("!1 "++str3)) of 
                     Fun f _ li ->  
                         if (willAppears!!bestInd || (and dones3))  `debug` "!2" --if it will appear again, or all done, put on top of stack.
                          then
                              let
                                  top4 = top3 + 1 - (length li)  `debug` "!3"--or length inps
                                  b4 = (b3 |> Data.Bimap.insert bestInd (top4))  `debug` ("!4 "++(show (str3, b3, btemp3, top3, dones3)))
                      -- this doesn't take it off.
                                  str4 = str3 ++ f ++ " "  `debug` "!5"
                             in (str4, b4, top4, dones3) `debug` "D3"
                          else
                              let 
                                  dones4 = listUpdate bestInd True dones3
                                  str4 = str3 ++ f ++ " OP_VERIFY "
                              in
                                (str4, b3, top3 - (length li), dones3) `debug` "D4"

willBeUsedAgain :: [[Int]] -> [Bool] -> Int -> Bool
willBeUsedAgain outs dones i =
    (length (Data.List.filter (\j -> not (dones!!j)) (outs!!i)) >= 1)    

--permanent BI, temporary BI
process :: Circuit Int -> [[Int]] -> [[Int]] -> Int -> Int -> (String, BI, BI, Int, [Bool]) -> (String, BI, BI, Int, [Bool])
process circ ins outs bestInd i (str, b, btemp, top, dones) =
    let
        --stack position
        sp = indexToStackPos b i
        d = dones!!i
        dones2 = listUpdate i True dones `debug` ("dones: "++show dones)
    in
        case circ!!i of
          Const x -> 
              let 
                  str2 = str ++ (show x) ++ " "
                  btemp2 = btemp |> Data.Bimap.insert i (top + 1)
                  top2= top + 1
              in
                (str2, b, btemp2, top2, dones2) `debug` ("1: "++show (str2, b, btemp2, top2, dones2))
          _ -> 
             if willBeUsedAgain outs dones i || (length $ Data.List.filter (==i) (ins!!bestInd)) > 1 --if it appears twice here
                then 
                    let
                        --PICK means copy
                        n = top - sp + 1
                        s = if n==1 then "OP_DUP" else show (top - sp) ++ " OP_PICK"
                        str2 = str ++ s ++ " "
                        btemp2 = btemp |> Data.Bimap.insert i (top + 1)
                        top2 = top + 1
                    in (str2, b, btemp2, top2, dones2) `debug` ("2: "++show (str2, b, btemp2, top2, dones2))
     --if done, then move (with consequences)
                else
                    let
                        str2 = str ++ show (top - sp) ++ " OP_ROLL "
                        b2 = b |> Data.Bimap.delete i |> bmapR (\x -> if x>sp then x-1 else x)
                        btemp2 = btemp |> bmapR (\x -> iflist [(x>sp, x-1),
                                                              (x==sp, top),
                                                              (x<sp, x)])
                    in (str2, b2, btemp2, top, dones2) `debug` ("3: "++show (str2, b2, btemp2, top, dones2))

bmapR:: (Ord b, Ord a) => (b -> b) -> Bimap a b -> Bimap a b
bmapR f bmap = fromList $ map (\(x,y) -> (x,f y)) (toList bmap)

postProcess:: String -> String
postProcess s = foldl (\x (y,z) -> replace y z x) s
                 (map (\x -> (x++" OP_VERIFY", x++"VERIFY")) 
                      ["OP_EQUAL","OP_NUMEQUAL","OP_CHECKSIG","OP_CHECKMULTISIG"])
--http://stackoverflow.com/questions/14907600/how-to-replace-a-string-with-another-in-haskell

