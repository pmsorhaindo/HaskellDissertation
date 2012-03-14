-- Prett-ify stuff [Cause not know where stuff is is pissing me off!]
module ConsoleView where
import AntRepresent
import Quadrant
import World
import GraphOps
import Data.Maybe (isNothing, isJust)


prettyAnt aGraph = do
        let theData = brokenUpGraph aGraph        
        let siz = truncate $ sqrt $fromIntegral $length $theData
        prizzle 1 siz theData
        

prizzle count siz (someData:theData) | isJust (fstTrip someData) && count`mod`siz == 0 = do putStr("|O|\n")
                                                                                            prizzle (count+1) siz theData

                                     | isJust (fstTrip someData) = do putStr("|O|")
                                                                      prizzle (count+1) siz theData

                                     | isNothing (fstTrip someData) && count `mod`siz == 0 = do putStr("|X|\n") 
                                                                                                prizzle (count+1) siz theData
                                     | isNothing (fstTrip someData) = do putStr("|X|")
                                                                         prizzle (count+1) siz theData
                                     | otherwise = do putStr("|X|")
                                                      prizzle (count+1) siz theData
prizzle count siz [] = putStrLn(".")


prettyAntWorld aWorld  = do
        let theData = brokenUpGraph aWorld   
        let siz = truncate $ sqrt $fromIntegral $length $theData
        printWorldLine 1 1 siz aWorld

printWorldLine :: Int -> Int-> Int -> GraphAWTuple -> IO()

printWorldLine row worldCol siz aWorld  | ((siz^2))-row == 0 = putStr("")

                                     {- | not((siz - worldCol+1) == 0) && ((row-1`mod`siz) == 0) = do
        putStrLn("")
        specificLinePrint aWorld row siz worldCol-}

                                        | not((siz - worldCol+1) == 0) = specificLinePrint aWorld row siz worldCol

                                        | otherwise = printWorldLine (row+1) 1 siz aWorld

specificLinePrint aWorld row siz worldCol = do
        --a <- getLine
        let preData = brokenUpGraph $ fstTrip ((sndTrip aWorld) ((worldCol-1)+row-1)) -- pulls out the specific Graph! so only siz^2 elements in pre Data!
        let theData = fst $ splitAt siz $snd (splitAt (nlf row siz) preData) -- TODO something wrong here with picking up rows
        putStr("World Col  = "++ (show worldCol) ++ " ")
        putStr("World Row  = " ++ (show row) ++ " ")
        putStr("World Size = " ++ (show siz) ++ " ")        
        prizzleLn worldCol siz theData
        printWorldLine row (worldCol+1) siz aWorld


prizzleLn :: Int -> Int -> [(Maybe Ant, Int, [Int])] -> IO ()
prizzleLn worldCol siz (someData:theData) | isJust (fstTrip someData) = do putStr("|O|")
                                                                           prizzleLn worldCol siz theData

                                     | isNothing (fstTrip someData) = do putStr("|X|")
                                                                         prizzleLn worldCol siz theData
                                     | otherwise = do putStr("|X|")
                                                      prizzleLn worldCol siz theData

prizzleLn worldCol siz [] | worldCol`mod`siz == 0 = putStrLn("")
                          | otherwise = putStr ("  ")

nlf :: Int -> Int -> Int
nlf row siz | row == 0 = 1
            | otherwise = (row)-(row`mod`siz)  -- next Lowest factor

hcf :: Integer -> Integer -> Integer
hcf a b | b==0      = abs a
        | otherwise = hcf b (a`mod`b)
