-- | This module produces an executable for the Ant Colony simulation, and
--   details the simulation loop.
--
module Main
    ( main
    ) where

import World
import Data.Foldable (forM_)


-- | Main function
--
main :: IO ()
main = do
        forM_ (iterate (`processAQuadrant` b) a) (print . brokenUpGraph)
        {-putStrLn $ show (brokenUpGraph a)
        let c = processAQuadrant a b
        putStrLn $ "\n" ++ show (brokenUpGraph c)
        let d = processAQuadrant c b
        putStrLn $ "\n" ++ show (brokenUpGraph d)
        let e = processAQuadrant d b
        putStrLn $ "\n" ++ show (brokenUpGraph e)
        let f = processAQuadrant e b
        putStrLn $ "\n" ++ show (brokenUpGraph d)
        let g = processAQuadrant f b
        putStrLn $ "\n" ++ show (brokenUpGraph g)
        let h = processAQuadrant g b
        putStrLn $ "\n" ++ show (brokenUpGraph h)
        let i = processAQuadrant h b
        putStrLn $ "\n" ++ show (brokenUpGraph i)
        let j = processAQuadrant i b
        putStrLn $ "\n" ++ show (brokenUpGraph j)
        let k = processAQuadrant j b
        putStrLn $ "\n" ++ show (brokenUpGraph k)-}
