module ListWorld where
--List world

import Ant
import Surface
import Food
import SimDefine
import Control.Monad.Writer  
import Control.Monad.RWS  
import System.Random

data AntList = AntList [Ant]
        deriving(Show)
data SurfaceList = SurfaceList [Surface]
        deriving(Show)
data FoodList = FoodList [Food]
        deriving(Show)

--newAntColonyLocation :: Int -> (Location,Direction)

genRandoms :: IO [Int]
genRandoms = do { g <- getStdGen; return $ randomRs (1, 100) g}

--random :: (MonadIO m) => m Int
random = liftIO $ randomRIO (1, 10)


--newLocation = do{ a<-ListWorld.random; b<-ListWorld.random;  c>>=Location  a b; return$ Ant c North} 
  
newLocation a b = Location a b      
newerLocation = Location 3 2

createAnt = Ant (newLocation 4 5) North

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


{--generateAnts :: Int -> AntList

--generateSurface ::



updateAnts :: AntList -> AntList

updateSurface :: SurfaceList -> SurfaceList

updateFood :: FoodList -> FoodList-}

