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

randomFloat :: RandomGen g => g -> ((Float), g)
randomFloat gen = let (a,gen') = random gen
                     in (a,gen')

randomFloats gen x [] =  do
                        let (a,gen1) = randomFloat gen
                        randomFloats gen1 (x-1) [a]

randomFloats gen 0 list = list

randomFloats gen x list = do
                        let a = randomFloat gen
                        randomFloats (snd a) (x-1) ((fst a):list)

randomFloats' n g = take n (randoms g)

-- | An infinite list of random numbers.
genRandoms :: IO [Int]
genRandoms = do { g <- getStdGen; return $ randomRs (1,4) g}



--randomlist :: Int -> StdGen -> [Int]
--randomlist n = take n . unfoldr (Just . random)

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
