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
import Control.Monad.State ( get, put, runState
                           , evalState, State(..)
                           )

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

setupSim worldSize quadSize antNum foodNum rs aidST = do
        let aw = newAWorld worldSize quadSize--new antWorld -para
        let pw = newPWorld worldSize quadSize--new pherWorld
        let fw = newFWorld worldSize quadSize--new foodWorld
        let nw = newNWorld worldSize quadSize--new nestWorld
        let ns = splitAt antNum $ take (antNum*2) rs
        let ns'= zip (fst ns) (snd ns)
        let aPopulate = populateAntWorld aw antNum ns' aidST
        let fPopulate = populateFoodWorld fw foodNum (snd aPopulate)
        let nPopulate = populateNestWorld nw (snd fPopulate) --set NestLoc
        --placeAnts -para
        --let fPopulate = --placeFood
        (aPopulate,pw,fPopulate,nPopulate)
        
        
        

-- | Main function
main :: IO ()
main = do
        -- Launch GUI.
        rnumbers <- genRandoms 1 8
        putStrLn("Testing IO .. input please ")        
        a <- getLine
        putStrLn ("IO Test :: " ++ a)
        putStrLn ("Testing crossing Quadrants")
        putStrLn (show $ fst $ splitAt 3 rnumbers)
        let antID = runState (do{return 1}) 1
        let sti = stitchUpEdge antWorld_ pherWorld 3 ((2,West),(3,East))
        prettyAnt $ fst $ antGraphs sti        
        let x = procEdgeAntAtNode 1 sti
        prettyAnt $ fst $ antGraphs x

        let sim = setupSim 4 4 10 3 rnumbers antID
        

        putStrLn("")
        putStrLn("")
        let atestAddWorld = newAWorld 3 3
        prettyAntWorld $ atestAddWorld
        putStrLn("")
        putStrLn("")
        let ns = splitAt 1000 $ take (1000*2) rnumbers
        let ns'= zip (fst ns) (snd ns)
        let aPopulate = populateAntWorld atestAddWorld 3 ns' antID
        prettyAntWorld $ fst aPopulate
        
        putStrLn("")
        --_ <- forkIO guiFunc
        --antVisGL
                

       
{-
       -}
