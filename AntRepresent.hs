module AntRepresent where

--Type enumeration
data Direction = North | South | East | West
        deriving(Eq)

data Mode = Explore | Return -- possibly add more behaviour types here
        deriving (Eq)

instance Show Direction where 
        show North = "North"
        show South = "South"
        show East  = "East"
        show West  = "West"

data Ant = Ant {
         antId     :: Int
        ,antDir    :: Direction
        ,pherLevel :: Double
        ,age       :: Int
        ,aim       :: Mode
        }
        deriving(Eq)

instance Show Ant where
         show (Ant {pherLevel = i} ) = "Ant: " ++ show i



oppDir x | x == North = South
         | x == South = North
         | x == East  = West
         | x == West  = East


--changeDir :: Vector -> Vector
--releasePheremone :: BodyCurrPher -> (PherType,PherStrength) -- This is called by the environment to update its pheromone concentration.
--senseSurroundings ::
