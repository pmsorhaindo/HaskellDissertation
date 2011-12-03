import ListWorld
import SimDefine
import Control.Monad.Writer  
import Control.Monad.RWS  
import System.Random

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
        
