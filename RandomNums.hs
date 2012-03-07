--Random Nums

import System.Random

{-main = do
        gen <- newStdGen
        let ns = randoms gen :: [Float]
        print $ take 10 ns -}

import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    askForNumber gen  
  
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

