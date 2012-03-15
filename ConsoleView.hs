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
        let wsiz = truncate $ sqrt $fromIntegral $length $theData
        printWorldLine 0 1 wsiz aWorld

printWorldLine :: Int -> Int-> Int -> GraphAWTuple -> IO()

printWorldLine row worldCol wsiz aWorld  | ((wsiz^2))-row == 0 = putStr("")

                                         | ((wsiz - worldCol) > -1) = do
        specificLinePrint aWorld row wsiz worldCol

                                         | otherwise = do
        printWorldLine row 1 wsiz aWorld

specificLinePrint aWorld row wsiz worldCol = do
        --a <- getLine
        let preData = brokenUpGraph $ fstTrip ((sndTrip aWorld) ((nlf row wsiz)+(worldCol-1))) -- pulls out the specific Graph! so only siz^2 elements in pre Data!
        let theData = fst $ splitAt wsiz $snd (splitAt ((row`mod`wsiz)*wsiz) preData) -- TODO wsiz chang to local size
        --putStr("World Col = "++ (show worldCol) ++ " ")
        --putStr("World Row = " ++ (show row) ++ " ")
        --putStr("World Siz = " ++ (show wsiz) ++ " ")
        --putStrLn("Test ")
        --prizzleLn worldCol wsiz preData
        --putStrLn("%")
        prizzleLn worldCol wsiz theData
        let moveOn = worldCol+1   
        if moveOn > wsiz
                then printWorldLine (row+1) (worldCol+1) wsiz aWorld
                else printWorldLine row (worldCol+1) wsiz aWorld


prizzleLn :: Int -> Int -> [(Maybe Ant, Int, [Int])] -> IO ()
prizzleLn worldCol siz (someData:theData) | isJust (fstTrip someData) = do putStr("|O|")
                                                                           prizzleLn worldCol siz theData

                                          | isNothing (fstTrip someData) = do putStr("|X|")
                                                                              prizzleLn worldCol siz theData
                                          | otherwise = do putStr("|X|")
                                                           prizzleLn worldCol siz theData

prizzleLn worldCol wsiz [] | worldCol`mod`wsiz == 0 = putStrLn("")
                           | otherwise = putStr ("  ")

nlf :: Int -> Int -> Int
nlf row siz = (row)-(row`mod`siz)  -- next Lowest factor

hcf :: Integer -> Integer -> Integer
hcf a b | b==0      = abs a
        | otherwise = hcf b (a`mod`b)
