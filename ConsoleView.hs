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

printWorldLine 0 worldCol siz aWorld = putStr("")

printWorldLine row worldCol siz aWorld | not((siz - worldCol+1) == 0) = do
        let preData = brokenUpGraph $ fstTrip ((sndTrip aWorld) (row-1))
        let theData = fst $ splitAt siz $snd (splitAt (siz*(row-1)) preData) -- TODO something wrong here with picking up ros
        prizzleLn worldCol siz theData
        printWorldLine row (worldCol+1) siz aWorld

                                       | otherwise = printWorldLine (row+1) 1 siz aWorld


prizzleLn :: Int -> Int -> [(Maybe Ant, Int, [Int])] -> IO ()
prizzleLn worldCol siz (someData:theData) | isJust (fstTrip someData) = do putStr("|O|")
                                                                           prizzleLn worldCol siz theData

                                     | isNothing (fstTrip someData) = do putStr("|X|")
                                                                         prizzleLn worldCol siz theData
                                     | otherwise = do putStr("|X|")
                                                      prizzleLn worldCol siz theData

prizzleLn worldCol siz [] | worldCol`mod`siz == 0 = putStrLn("")
                          | otherwise = putStr ("  ")
