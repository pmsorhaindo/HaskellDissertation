module AntRepresent where

--Representation of an ANT

type LocX = Double
type LocY = Double
type LocZ = Double
type DiffX = Double
type DiffY = Double
type DiffZ = Double
type Speed = Double 
type PherType = Int
type PherStrength = Double

--Type enumeration
data Direction = North | South | East | West


instance Show Direction where 
        show North = "North"
        show South = "South"
        show East = "East"
        show West = "West"


data Location = Location {
        x :: Int,
        y :: Int
        } 
	deriving(Show)

data Vector = Vector {
        direction :: Direction,
        magnitude :: Int
        }
	deriving(Show)

--data BodyCurrPher = BodyCurrPher (PherType,PherStrength)
--	deriving(Show)

data Ant = Ant Location Vector
        deriving(Show)

getAntLocation :: Ant -> Location
getAntLocation (Ant l _) = l

getAntVector :: Ant -> Vector
getAntVector (Ant _ v) = v


--A complex 
--amy = Ant (Location 0.0,0.1,0.5) (Vector 0.0,0.4,0.0)  (BodyCurrPher 3,10.0)
amy2 :: Ant
amy2 = Ant (Location 0 1) (Vector North 2)

moveAntUp:: Ant -> Ant
moveAntUp (Ant x y) = Ant (moveUp x) y

moveAntDown:: Ant -> Ant
moveAntDown (Ant x y) = Ant (moveDown x) y

moveAntRight:: Ant -> Ant
moveAntRight (Ant x y) = Ant (moveRight x) y

moveAntLeft:: Ant -> Ant
moveAntLeft (Ant x y) = Ant (moveLeft x) y

moveUp :: Location -> Location
moveUp (Location x y) = Location x (y+1)

moveDown :: Location -> Location
moveDown (Location x y) = Location x (y-1)

moveLeft :: Location -> Location
moveLeft (Location x y) = Location (x+1) y

moveRight :: Location -> Location
moveRight (Location x y) = Location (x-1) y


--changeDir :: Vector -> Vector
--releasePheremone :: BodyCurrPher -> (PherType,PherStrength) -- This is called by the environment to update its pheromone concentration.
--senseSurroundings ::
