--Random Nums
module RandomNums where
import System.Random
import System.Random  
import Control.Monad(when)
import Control.Monad.State 

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom = get >>= \gen ->
        let (val, gen') = random gen in
        put gen' >>
        return val

getTwoRandoms :: Random a => RandomState (a,a)
getTwoRandoms = liftM2 (,) getRandom getRandom

printxRandoms x = do
        gen <- newStdGen
        let ns = randoms gen :: [Float]
        print $ take x ns

twoBadRandoms :: RandomGen g => g -> (Int,Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

refreshRandoms = newStdGen

twoGoodRandoms :: RandomGen g => g -> ((Float), g)
twoGoodRandoms gen = let (a,gen') = random gen
                     in (a,gen')


{-
-- modified random number game with flexible range
askForNumber :: StdGen -> IO ()  
askForNumber gen = do 
    putStr "Set your range: "
    sizStr <- getLine
    let siz = read sizStr
    let (randNumber, newGen) = randomR (1,siz) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number   
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen
-}
