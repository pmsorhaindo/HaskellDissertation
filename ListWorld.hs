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

data World = {
        get_antList :: [Ant],
        get_surfaceList :: [Surface],
        get_foodList :: [Food],
        get_rNumbers :: [Int]
        } deriving(Show,Eq,Read)

--newAntColonyLocation :: Int -> (Location,Direction)

newRandom :: (MonadIO m) => m Int
newRandom = liftIO $ randomRIO (1, 10)


--newLocation = do{ a<-ListWorld.random; b<-ListWorld.random;  c>>=Location  a b; return$ Ant c North} 
  
newLocation a b = Location a b      
newerLocation = Location 3 2

createAnt = Ant (newLocation 4 5) North

createAnt' = do
        x<-newRandom
        y<-newRandom
        putStr $ show x
        putStr " no. ant is ready.\n"

createAntLoop :: Integer -> [Ant]
createAntLoop decr -- decrimenting parameter for a loop
        | decr < 1 = [] -- ends the loop
        | decr < colonyCapacity = createAnt : createAntLoop (decr-1)
        | decr > colonyCapacity = []


createFood = Food newerLocation 40

createFoodLoop :: Integer -> [Food]
createFoodLoop decr
        | decr < 1 = []
        | decr < colonyCapacity = createFood : createFoodLoop (decr-1)
        | decr > colonyCapacity = []


generateWorld :: [Int] -> World
generateWorld random =
  let parameters = getParameters val
      (world, randomVals) = runState (createWorld parameters) random
      world = 
      gs = GameState world ent ent True False 5 0 randomVals
  in if isGameSolvable gs then gs else createRandomGame d randomVals
  where
    swap (a, b)            = (b, a)
    locateEntrance g       = lookup Entrance $ map swap $ assocs g
    
generateSurface :: SimParameters -> State [Int] [Surface]
generateSurface (SimParameters size ants capacity food) =
  let initialSurface = listArray ((1, 1), (x, y)) $ repeat StableDry
  in insertTilesRandomly Wet (size/5) initialGrid
     >>= insertTilesRandomly UnstableDry (size)



generateAnts :: SimParameters -> State [Int] [Surface]
generateAnts (SimParameters size ants capacity food) =
  let initialSurface = listArray ((1, 1), (x, y)) $ repeat StableDry
  in randomTerrain UnstableDry w initialGrid
     >>= randomTerrain Wet ()

generateFood :: SimParameters -> State [Int] [Surface]
generateFood (SimParameters size ants capacity food) =
  let initialSurface = listArray ((1, 1), (x, y)) $ repeat StableDry
  in randomTerrain UnstableDry w initialGrid
     >>= randomTerrain Wet ()

--Randomly add varied terrain with the extraction of Random numbers.
randomTerrainPlacement ::  
randomTerrainPlacement _ 0 g = return g
randomTerrainPlacement surface n g = do
  (x:y:randomVals) <- get -- pulling 2 random value from State Monad
  put randomVals          -- updating random value state
  let (_, (x', y')) = bounds g
  case addSpecialTile g (x `mod` x' + 1, y `mod` y' + 1) t of
    Nothing -> randomTerrainPlacement surface amount surfaceList
    Just surfaceList' -> randomTerrainPlacemenet surface (amount-1) surfaceList'

{--generateAnts :: Int -> AntList

--generateSurface ::



updateAnts :: AntList -> AntList

updateSurface :: SurfaceList -> SurfaceList

updateFood :: FoodList -> FoodList-}

