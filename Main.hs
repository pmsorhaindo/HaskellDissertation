import ListWorld
import SimDefine
import Control.Monad.Writer  
import Control.Monad.RWS  
import System.Random

genRandoms :: IO [Int]
genRandoms = do { g <- getStdGen; return $ randomRs (1, 100) g}
x = createAntLoop 3
y = show Main.x ++ "\n"
main = do
        putStr $ show worldSize
        --generateAnts startAntAmount
        --x >>= createAntLoop 3
        putStr Main.y
        --nums <- genRandoms
        --x >>= nums!!2
        --putStr (read x)
        --putStr "testing"
        
