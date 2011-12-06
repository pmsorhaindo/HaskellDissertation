module SimDefine where

data Location = Location {
        xpos :: Int,
        ypos :: Int
        } 
	deriving(Show)

data Settings = Simple | Interesting | Complex

data SimParameters = SimParameters {
        get_worldSize :: Int,
        get_startAntAmount :: Int,
        get_colonyCapacity :: Int,
        get_amountOfFood :: Int
        } deriving(Show)

data Direction = North | South | East | West

instance Show Direction where 
        show North = "North"
        show South = "South"
        show East = "East"
        show West = "West"

getParameters Simple   = SimParameters 10 5 10 2
getParameters Interesting = SimParameters 20 15 30 4
getParameters Complex   = SimParameters 60 40 80 10

--For the old code
worldSize = 10
startAntAmount = 3
colonyCapacity = 5
amountOfFood = 5
