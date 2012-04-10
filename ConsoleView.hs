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
        let qsiz = truncate $ sqrt $fromIntegral $length $brokenUpGraph $ fstTrip ((sndTrip aWorld) (0))
        printWorldLine 0 1 wsiz qsiz aWorld

printWorldLine :: Int -> Int-> Int -> Int -> GraphAWTuple -> IO()

printWorldLine row worldCol wsiz qsiz aWorld  | ((wsiz*qsiz))-row == 0 = putStr("")

                                              | ((wsiz - worldCol) > -1) = do
        specificLinePrint aWorld row wsiz qsiz worldCol

                                              | otherwise = do
        printWorldLine row 1 wsiz qsiz aWorld

getGraph row qsiz wsiz worldCol = ((row`div`qsiz)*wsiz)+(worldCol-1)

specificLinePrint aWorld row wsiz siz worldCol = do
        --a <- getLine
        --let siz = truncate $ sqrt $fromIntegral $length $brokenUpGraph $ fstTrip ((sndTrip aWorld) (0)) 
        let preData = brokenUpGraph $ fstTrip ((sndTrip aWorld) (getGraph row siz wsiz worldCol)) -- pulls out the specific Graph! so only siz^2 elements in pre Data! 
        let theData = fst $ splitAt siz $snd (splitAt ((row`mod`siz)*siz) preData) -- TODO wsiz chang to local size
        --putStr("World Col = "++ (show worldCol) ++ " ")
        --putStr("World Row = " ++ (show row) ++ " ")
        --putStr("World Siz = " ++ (show wsiz) ++ " ")
        --putStrLn("Test ")
        --prizzleLn worldCol wsiz preData
        --putStrLn("%")
        prizzleLn worldCol wsiz siz row theData
        let moveOn = worldCol+1   
        if moveOn > wsiz
                then printWorldLine (row+1) (worldCol+1) wsiz siz aWorld
                else printWorldLine row (worldCol+1) wsiz  siz aWorld


prizzleLn :: Int -> Int -> Int -> Int -> [(Maybe Ant, Int, [Int])] -> IO ()
prizzleLn worldCol wsiz siz row (someData:theData) | isJust (fstTrip someData) = do putStr("|O|")
                                                                                    prizzleLn worldCol wsiz siz row theData

                                                   | isNothing (fstTrip someData) = do putStr("|X|")
                                                                                       prizzleLn worldCol wsiz siz row theData
                                                   | otherwise = do putStr("|X|")
                                                                    prizzleLn worldCol wsiz siz row theData

prizzleLn worldCol wsiz siz row [] | (worldCol`mod`wsiz == 0) && (row`mod`siz == (siz-1)) = putStrLn("\n")
                                   | worldCol`mod`wsiz == 0 = putStrLn("")
                                   | otherwise = putStr ("  ")

nlf :: Int -> Int -> Int
nlf row siz = (row)-(row`mod`siz)  -- next Lowest factor

hcf :: Integer -> Integer -> Integer
hcf a b | b==0      = abs a
        | otherwise = hcf b (a`mod`b)




prettyPherWorld pWorld  = do
        let theData = brokenUpGraph pWorld   
        let wsiz = truncate $ sqrt $fromIntegral $length $theData
        printPWorldLine 0 1 wsiz pWorld

printPWorldLine :: Int -> Int-> Int -> GraphPWTuple -> IO()

printPWorldLine row worldCol wsiz pWorld  | ((wsiz*3))-row == 0 = putStr("")

                                         | ((wsiz - worldCol) > -1) = do
        specificPLinePrint pWorld row wsiz worldCol

                                         | otherwise = do
        printPWorldLine row 1 wsiz pWorld


specificPLinePrint pWorld row wsiz worldCol = do
        let siz = truncate $ sqrt $fromIntegral $length $brokenUpGraph $ fstTrip ((sndTrip pWorld) (0)) 
        let preData = brokenUpGraph $ fstTrip ((sndTrip pWorld) (getGraph row siz wsiz worldCol)) -- pulls out the specific Graph! so only siz^2 elements in pre Data! 
        let theData = fst $ splitAt siz $snd (splitAt ((row`mod`siz)*siz) preData)
        prizzlePLn worldCol wsiz siz row theData
        let moveOn = worldCol+1   
        if moveOn > wsiz
                then printPWorldLine (row+1) (worldCol+1) wsiz pWorld
                else printPWorldLine row (worldCol+1) wsiz pWorld

--prizzlePLn :: Int -> Int -> Int -> Int -> [(Maybe Ant, Int, [Int])] -> IO ()
prizzlePLn worldCol wsiz siz row (someData:theData) = do putStr("|"++ show(fstTrip someData)++"|")
                                                         prizzlePLn worldCol wsiz siz row theData


prizzlePLn worldCol wsiz siz row [] | (worldCol`mod`wsiz == 0) && (row`mod`siz ==2) = putStrLn("\n")
                                   | worldCol`mod`wsiz == 0 = putStrLn("")
                                   | otherwise = putStr ("  ")
