import ListWorld
import SimDefine
import Control.Monad.Writer  
import Control.Monad.RWS  
import System.Random

main = do
    --Initialize the world

        putStr $ "The world's size is " ++ show worldSize ++ "\n"

        --generateSurface initialWorldSize
        
        --generateAnts startAntAmount
        let antList = createAntLoop 3
        putStr $ show antList ++ "\n"

        --generateFood
        let foodList = createFoodLoop 2


    --Caluclate next move
        --calculateNextMove :: [Ants] -> [Ants]
    --Generate Collision List
        --collidingAnts :: [Ants] -> [Ants]



        --x >>= createAntLoop 3
        --nums <- genRandoms
        --x >>= nums!!2
        --putStr (read x)
        --putStr "testing"
        putStr "Successful troll is successful.\n"
