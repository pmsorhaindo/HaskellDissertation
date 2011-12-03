module SimDefine where

data Location = Location {
        x :: Int,
        y :: Int
        } 
	deriving(Show)

worldSize = 10
startAntAmount = 3
colonyCapacity = 5

data Direction = North | South | East | West

instance Show Direction where 
        show North = "North"
        show South = "South"
        show East = "East"
        show West = "West"
