{-# OPTIONS -Wall -XRankNTypes #-}
-- | This module produces an executable for the Ant Colony simulation, and
--   details the simulation loop.
--
module Main
    ( main
    ) where


import Data.Foldable (forM_)
import Control.Parallel.Strategies
import Control.DeepSeq 
import Control.Concurrent
--import Control.Exception
--import Control.Applicative

import GUI
import SimDefine
import World
import Quadrant
import AntRepresent -- this is just so I can call directions with out Qualifying them in ghci
import GraphOps
import RandomNums
import ConsoleView
import QuadStitching
import Visuals
--type check
deep :: forall b. NFData b => b -> b
deep a = deepseq a a


--creates an infinite list where the first item is calculated by applying the function on the secod argument, the second item by 
--applying the function on the previous result and so on
--forM_ (iterate (`processAQuadrant` b) a) (print . brokenUpGraph)
--evaluate $ deep map runSim (quads `using` parList) rseq
runSimSingle :: GraphPTuple -> GraphATuple -> IO ()
runSimSingle pherG antG = forM_ (iterate (`processAQuadrant_` pherG) antG) (prettyAnt)

runSimParallel_fail :: forall t a. t -> [a] -> [a]
runSimParallel_fail aQuadrants pQuadrants = (pQuadrants `using` parList rpar)

runSimParallel_ :: forall t. [GraphATuple] -> t -> [GraphPTuple -> GraphATuple]
runSimParallel_ aQuadrants pQuadrants = (map processAQuadrant_ aQuadrants) `using` rpar 

--runSimParallel :: [(GraphATuple, GraphPTuple)] -> [GraphATuple]
--runSimParallel zippedQuads = parMap rpar (processAQuadrant_) zippedQuads --(map processAQuadrant aQuads)

runSimParallel :: [(GraphATuple, GraphPTuple)] -> [Int] ->  [GraphATuple]
runSimParallel zippedQuads noProcs  = parMap rpar (processAQuadrant noProcs) zippedQuads --(map processAQuadrant aQuads)


--(pQuads `using` parMap rpar processAQuadrant aQuads) 

aQuads :: [GraphATuple]
aQuads  = (listOfNodes $ brokenUpGraph antWorld) :: [GraphATuple]
pQuads :: [GraphPTuple]
pQuads  = listOfNodes $ brokenUpGraph pherWorld :: [GraphPTuple]

zippedQuads :: [(GraphATuple, GraphPTuple)]
zippedQuads = zip aQuads pQuads

z :: GraphATuple
z = processAQuadrant_ (head aQuads) emptyPherQuadrant 


-- | Main function
main :: IO ()
main = do
        -- Launch GUI.
        rnumbers <- genRandoms 1 4
        a <- getLine
        putStrLn ("Hey " ++ a)
        putStrLn ("test")
        putStrLn (show $ fst $ splitAt 3 rnumbers)
        let sti = stitchUpEdge antWorld_ pherWorld 3 ((2,West),(3,East))
        prettyAnt $ fst $ antGraphs sti        
        let x = procEdgeAntAtNode 1 sti
        prettyAnt $ fst $ antGraphs x
        forkIO guiFunc
        antVisGL
        

        

--setUpWorld = 


        

       
