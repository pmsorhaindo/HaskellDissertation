-- Prett-ify stuff [Cause not know where stuff is is pissing me off!]
module ConsoleView where
import AntRepresent
import Quadrant
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
        let siz = 3      
        printWorldLine 1 1 siz aWorld

printWorldLine row worldCol siz aWorld = do
        let preData = brokenUpGraph $ fstTrip ((sndTrip aWorld) worldCol)
        let theData = fst $ splitAt siz $snd (splitAt (siz*row-1) preData)
        prizzleLn theData
        printWorldLine row (worldCol+1) siz aWorld
         
prizzleLn worldCol siz (someData:theData) | isJust (fstTrip someData) = do putStr("|O|")
                                                                           prizzleLn worldCol siz theData

                                     | isNothing (fstTrip someData) = do putStr("|X|")
                                                                         prizzleLn worldCol siz theData
                                     | otherwise = do putStr("|X|")
                                                      prizzleLn worldCol siz theData

prizzleLn worldCol siz [] | worldCol`mod`siz == 0 = putStrLn("")
                          | otherwise = putStr ("  ")
