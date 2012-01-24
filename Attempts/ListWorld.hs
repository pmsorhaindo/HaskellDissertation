module ListWorld where
--List world

import Ant
import Surface
import Food
import SimDefine
import Control.Monad.Writer  
import Control.Monad.RWS  
import System.Random
import Control.Monad.State

data AntList = AntList [Ant]
        deriving(Show)
data SurfaceList = SurfaceList [Surface]
        deriving(Show)
data FoodList = FoodList [Food]
        deriving(Show)

data World = World {
        get_antList :: [Ant],
        get_surfaceList :: [Surface],
        get_foodList :: [Food],
        get_rNumbers :: [Int]
        } deriving(Show)


--newAntColonyLocation :: Int -> (Location,Direction)

newRandom :: (MonadIO m) => m Int
newRandom = liftIO $ randomRIO (1, 10)

newLocation' = do{ a <- newRandom; b <- newRandom; c <- Location  a b; return$ Ant c North} 
  
newLocation a b = Location a b      
newerLocation = Location 3 2

createAnt = Ant (newLocation 4 5) North

createAnt' = do
        x<-newRandom
        y<-newRandom
        putStr $ show x
        putStr $ ":" $ show y
        putStr " coord. ant is ready.\n"

createAntLoop :: Integer -> [Ant]
createAntLoop decr -- decrimenting parameter for a loop
        | decr < 1 = [] -- ends the loop
        | decr < colonyCapacity = createAnt : createAntLoop (decr-1)
        | decr > colonyCapacity = []

{-generateWorld :: Settings -> [Int] -> World
generateWorld setting random =
  let parameters = getParameters setting
      (newAnts, randomVals) = runState (generateAnts parameters) random
      (newSurface, randomVals') = runState (generateSurface parameters) randomVals    
      (newFood, randomVals'') = runState (generateFood parameters) randomVals'             
      world = World newAnts newSurface newFood randomVals
  in if True then world else generateWorld setting randomVals'' -- loop back function needed incase the terrain is blocking all the ant from food. -}

coords sizex 0 = zip [0..sizex] (take sizex $repeat 0)
coords sizex sizey = zip [0..sizex] (take sizex $repeat sizey ) ++ coords sizex (sizey-1)

{- moved to Surface.hs
newSurfs :: [(a,b)] -> [Surface] -- TODO issue need to find a way to convert from integer to ints. (IF POSSIBLE :/)
newSurfs ((x,y):xs) = (StableDry (Location x  y)): (newSurfs xs)
newSurfs _ = [] -}

    
{-generateSurface :: SimParameters -> State [Int] [Surface]
generateSurface (SimParameters size ants capacity food) =
  let initialSurface = coords size size -- TODO need to 'zip' this with newSurfs
  in randomTerrainPlacement Wet (size/5) initialSurface
     >>= randomTerrainPlacement UnstableDry (size) -}


--Randomly add varied terrain with the extraction of Random numbers.
{-randomTerrainPlacement :: Surface -> Int -> [Surface] -> State [Int] [Surface]
randomTerrainPlacement _ 0 surfaceList = return surfaceList -- stores the surfaceList in the StateMonad
randomTerrainPlacement surface num surfaceList = do
  (x:y:randomVals) <- get                       -- pulling 2 random value from State Monad
  put randomVals                                -- updating random value state
  let boundaries = sqrt fromIntegral (length surfaceList)
  case addSpecialSurface surfaceList (x `mod` boundaries + 1, y `mod` boundaries+ 1) surface of -- return a modified surface list
    Nothing -> randomTerrainPlacement surface amount surfaceList
    Just surfaceList' -> randomTerrainPlacemenet surface (amount-1) surfaceList'-}


{-generateAnts :: SimParameters -> State [Int] [Ant] --TODO random ants using State Monad
generateAnts (SimParameters size ants capacity food) =
  let colonyOrigin = take ants (repeat Location 0 0 North)
  in randomLocate  -}

createFood = Food newerLocation 40

createFoodLoop :: Integer -> [Food]
createFoodLoop decr
        | decr < 1 = []
        | decr < colonyCapacity = createFood : createFoodLoop (decr-1)
        | decr > colonyCapacity = []

--generateFood :: SimParameters -> State [Int] [Food] --TODO random food generation using state Monad.
--generateFood (SimParameters size ants capacity food) = [Food]
  --let initialSurface = do
           -- \x = take food (repeat Food randomVal, randomVal) -}


{--generateAnts :: Int -> AntList

--generateSurface ::



updateAnts :: AntList -> AntList

updateSurface :: SurfaceList -> SurfaceList

updateFood :: FoodList -> FoodList-}
