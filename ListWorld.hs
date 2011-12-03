module ListWorld where
--List world

import Ant
import Surface
import Food
import SimDefine

data AntList = AntList [Ant]
        deriving(Show)
data SurfaceList = SurfaceList [Surface]
        deriving(Show)
data FoodList = FoodList [Food]
        deriving(Show)


--generateAnts :: Int -> AntList

--newAntColonyLocation :: Int ->

newLocation = Location 1 2
createAnt = Ant newLocation SimDefine.North

createAntLoop :: Integer -> [Ant]
createAntLoop acc
        | acc < 1 = []
        | acc < colonyCapacity = createAnt : createAntLoop (acc-1)
        | acc > colonyCapacity = []

{-generateSurface ::

generateFood ::

updateAnts :: AntList -> AntList

updateSurface :: SurfaceList -> SurfaceList

updateFood :: FoodList -> FoodList-}

