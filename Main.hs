-- | This module produces an executable for the Ant Colony simulation, and
--   details the simulation loop.
--
module Main
    ( main
    ) where

import World
import Quadrant
import Data.Foldable (forM_)


-- | Main function
--

--type check
temp = forM_ (iterate (`processAQuadrant` b) a) (print . brokenUpGraph)

main :: IO ()
main = do
        --creates an infinite list where the first item is calculated by applying the function on the secod argument, the second item by applying the function on the previous result and so on
        forM_ (iterate (`processAQuadrant` b) a) (print . brokenUpGraph)
       
         {-putStrLn $ show (brokenUpGraph a)
        let c = processAQuadrant a b
        putStrLn $ "\n" ++ show (brokenUpGraph c)
        let d = processAQuadrant c b
        putStrLn $ "\n" ++ show (brokenUpGraph d)
        let e = processAQuadrant d b
        putStrLn $ "\n" ++ show (brokenUpGraph e)
        ... ect-}
