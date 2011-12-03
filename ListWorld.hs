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

random :: (MonadIO m) => m Int
random = liftIO $ randomRIO (1, 10)

--randomval = do { l <- ListWorld.random; newLocation l l}

newLocation :: Int -> Int -> Location
newLocation a b = Location  a b 
        
newerLocation = Location 3 2

createAnt = Ant (newLocation 4 5) North

createAntLoop :: Integer -> [Ant]
createAntLoop acc
        | acc < 1 = []
        | acc < colonyCapacity = createAnt : createAntLoop (acc-1)
        | acc > colonyCapacity = []


{--generateAnts :: Int -> AntList

--generateSurface ::

generateFood ::

updateAnts :: AntList -> AntList

updateSurface :: SurfaceList -> SurfaceList

updateFood :: FoodList -> FoodList-}

