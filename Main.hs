-- | This module produces an executable for the Ant Colony simulation, and
--   details the simulation loop.
--
module Main
    ( main
    ) where

import World
import Quadrant
import AntRepresent -- this is just so I can call directions with out Qualifying them in ghci
import GraphOps
import ConsoleView
import Data.Foldable (forM_)
import Control.Parallel.Strategies
import Control.Exception
import Control.DeepSeq 
import Control.Applicative


--type check

deep a = deepseq a a

runSimSingle pherG antG = forM_ (iterate (`processAQuadrant` pherG) antG) (prettyAnt)

runSimParallel_fail aQuads pQuads = (pQuads `using` parList rpar)
runSimParallel_ aQuads pQuads = (map processAQuadrant aQuads) `using` rpar 

runSimParallel zippedQuads = parMap rpar (processAQuadrant_) zippedQuads --(map processAQuadrant aQuads)

--(pQuads `using` parMap rpar processAQuadrant aQuads) 


aQuads  = (listOfNodes $ brokenUpGraph antWorld) :: [GraphATuple]
pQuads  = listOfNodes $ brokenUpGraph pherWorld :: [GraphPTuple]
zippedQuads = zip aQuads pQuads

z = processAQuadrant (head aQuads) emptyPherQuadrant 


-- | Main function
main :: IO ()
main = do
        putStrLn ("test")
        --creates an infinite list where the first item is calculated by applying the function on the secod argument, the second item by applying the function on the previous result and so on
        --forM_ (iterate (`processAQuadrant` b) a) (print . brokenUpGraph)
        --evaluate $ deep map runSim (quads `using` parList) rseq

       
