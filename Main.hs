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
import Data.Foldable (forM_)
import Control.Parallel.Strategies
import Control.Exception
import Control.DeepSeq 


--type check

deep a = deepseq a a

runSim = forM_ (iterate (`processAQuadrant` b_) a_) (print . brokenUpGraph)
aQuads  = listOfNodes $ brokenUpGraph antWorld
pQuads  = listOfNodes $ brokenUpGraph pherWorld

z = processAQuadrant (head aQuads) emptyPherQuadrant 


-- | Main function
main :: IO ()
main = do
        putStrLn ("test")
        --creates an infinite list where the first item is calculated by applying the function on the secod argument, the second item by applying the function on the previous result and so on
        --forM_ (iterate (`processAQuadrant` b) a) (print . brokenUpGraph)
        --evaluate $ deep map runSim (quads `using` parList) rseq

       
