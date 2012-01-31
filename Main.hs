-- | This module produces an executable for the Ant Colony simulation, and
--   details the simulation loop.
--
module Main
    ( main
    ) where

import World
import Quadrant
import AntRepresent -- this is just so I can call directions with out Qualifying them in ghci
import Data.Foldable (forM_)


-- | Main function
--

--type check
temp = forM_ (iterate (`processAQuadrant` b_) a_) (print . brokenUpGraph)

main :: IO ()
main = do
        --creates an infinite list where the first item is calculated by applying the function on the secod argument, the second item by applying the function on the previous result and so on
        forM_ (iterate (`processAQuadrant` b) a) (print . brokenUpGraph)

        --evaluate $ deep $ map solve grids ‘ using ‘ parList rseq

       
