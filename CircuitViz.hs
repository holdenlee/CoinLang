module CircuitViz where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

import Utilities
import Circuit
import DAGViz
import Compiler

import Data.Graph.Inductive
--graph visualization
import Data.GraphViz
import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Attributes.Complete

circToGraph:: Circuit Int -> Gr String Int
circToGraph circ = 
    let
        ins = map (\x -> case x of Fun s _ li -> map removeArg li
                                   _ -> []
                 ) circ
        nodeList = zip [0..] $ map show circ
        edgeList = concat $ map (\(i, li) -> map (\(x,j) -> (x,i,j)) (zip li [0..])) (zip [0..] ins)
    in mkGraph nodeList edgeList 

graphToDot':: Gr String Int -> String
graphToDot' g = toDotString $ defaultVis (\n x _ -> (show n)++ ": "++ x) g
--(Node -> nl  -> Gr nl el-> String) -> Gr nl el

--use this one!
generateDotGraph :: Circuit2 Int -> String
generateDotGraph c = c |> preProcess |> circToGraph |> graphToDot'
