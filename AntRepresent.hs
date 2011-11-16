module AntRepresent  
( moveUp  
, moveDown  
, moveLeft  
, moveRight
, move
, changeDir 
, releasePheremone  
, senseSurroundings
) where  

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

data Location = Location (LocX,LocY,LocZ)
	deriving(Show)
data Vector = Vector (DiffX,DiffY,DiffZ) Speed
	deriving(Show)
data BodyCurrPher = BodyCurrPher (PherType,PherStrength)
	deriving(Show)

data Ant = Ant Location Vector BodyCurrPher

amy = Ant (0.0,0.1,0.5) (0.0,0.4,0.0) 3 (3,10.)

moveUp :: Location -> Location
--moveUp :: 
moveDown :: Location -> Location
moveLeft :: Location -> Location
moveRight :: Location -> Location
move :: Location -> Location
changeDir :: Vector -> Vector
releasePheremone :: BodyCurrPher -> (PherType,PherStrength) -- This is called by the environment to update its pheromone concentration.
--senseSurroundings ::
