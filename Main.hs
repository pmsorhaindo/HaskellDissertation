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
        let ns = splitAt antNum $ take (antNum*2) rs
        let ns'= zip (fst ns) (snd ns)
        let aPopulate = populateAntWorld aw antNum ns' aidST
        ns'
        --set NestLoc
        --placeAnts -para
        --placeFood

-- | Main function
main :: IO ()
main = do
        -- Launch GUI.
        rnumbers <- genRandoms 1 4
        a <- getLine
        putStrLn ("Hey " ++ a)
        putStrLn ("test")
        putStrLn (show $ fst $ splitAt 3 rnumbers)
        let antID = runState (do{return 1}) 1
        let sti = stitchUpEdge antWorld_ pherWorld 3 ((2,West),(3,East))
        prettyAnt $ fst $ antGraphs sti        
        let x = procEdgeAntAtNode 1 sti
        prettyAnt $ fst $ antGraphs x

        let sim = setupSim 4 4 10 3 rnumbers antID
        

        putStrLn("")
        putStrLn("")
        let atestAddWorld = newAWorld 5 5
        prettyAntWorld $ atestAddWorld
        putStrLn("")
        putStrLn("")
        let ns = splitAt 3 $ take (3*2) rnumbers
        let ns'= zip (fst ns) (snd ns)
        let aPopulate = populateAntWorld atestAddWorld 3 ns' antID
        prettyAntWorld $ aPopulate
        
        putStrLn("")
        --_ <- forkIO guiFunc
        --antVisGL
                

       
{-
       -}
