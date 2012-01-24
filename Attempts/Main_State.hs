import ListWorld
import SimDefine
import Control.Monad.Writer  
import Control.Monad.RWS
import Control.Monad.State
import System.Random

genRandoms :: IO [Int]
genRandoms = do { g <- getStdGen; return $ randomRs (1, 100) g}

main = do
    --Initialize the world
        numbers <- genRandoms
        putStr $ "The world's size is " ++ show worldSize ++ "\n"

        --generateSurface initialWorldSize
        --x >>= createAntLoop 3
        --nums <- genRandoms
        --generateAnts startAntAmount

        let antList = createAntLoop 3
        putStr $ show antList ++ "\n"
        --generateFood
        let foodList = createFoodLoop 2
        
        let worldInit = generateWorld numbers
        simLoop worldInit

simLoop :: World -> IO()
simLoop = do

--Caluclate next move
        --calculateNextMove :: [Ants] -> [Ants]
--Generate Collision List
        --collidingAnts :: [Ants] -> [Ants]

        --x >>= nums!!2
        --putStr (read x)
        putStr "Simulation RUNNING!.\n"
        simLoop
    
