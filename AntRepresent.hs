module AntRepresent where

--Type enumeration
data Direction = North | South | East | West
        deriving(Eq)


instance Show Direction where 
        show North = "North"
        show South = "South"
        show East = "East"
        show West = "West"

data Ant = Ant {
         antId        :: Int
        ,dir       :: Direction
        ,pherLevel :: Double
        }
        deriving(Eq)

instance Show Ant where
         show (Ant {antId = i}) = "Ant: " ++ show i





--changeDir :: Vector -> Vector
--releasePheremone :: BodyCurrPher -> (PherType,PherStrength) -- This is called by the environment to update its pheromone concentration.
--senseSurroundings ::
