module AntRepresent where

--Representation of an AntOld

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
        deriving(Eq)


instance Show Direction where 
        show North = "North"
        show South = "South"
        show East = "East"
        show West = "West"


data Location = Location {
        x :: Int,
        y :: Int
        } 
	deriving(Show,Eq)

data Vector = Vector {
        direction :: Direction,
        magnitude :: Int
        }
	deriving(Show,Eq)

--data BodyCurrPher = BodyCurrPher (PherType,PherStrength)
--	deriving(Show)

data Ant = Ant Int Direction
        deriving(Show,Eq)

data AntOld = AntOld Location Vector
        deriving(Show,Eq)

getAntOldLocation :: AntOld -> Location
getAntOldLocation (AntOld l _) = l

getAntOldVector :: AntOld -> Vector
getAntOldVector (AntOld _ v) = v


--A complex 
--amy = AntOld (Location 0.0,0.1,0.5) (Vector 0.0,0.4,0.0)  (BodyCurrPher 3,10.0)
amy2 :: AntOld
amy2 = AntOld (Location 0 1) (Vector North 2)

moveAntOldUp:: AntOld -> AntOld
moveAntOldUp (AntOld x y) = AntOld (moveUp x) y

moveAntOldDown:: AntOld -> AntOld
moveAntOldDown (AntOld x y) = AntOld (moveDown x) y

moveAntOldRight:: AntOld -> AntOld
moveAntOldRight (AntOld x y) = AntOld (moveRight x) y

moveAntOldLeft:: AntOld -> AntOld
moveAntOldLeft (AntOld x y) = AntOld (moveLeft x) y

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
