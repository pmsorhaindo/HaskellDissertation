module AntRepresent where
import SimDefine

data Mode = Explore | Return -- possibly add more behaviour types here
        deriving (Eq)

data Ant = Ant {
         antId     :: Int
        ,antDir    :: Direction
        ,pherLevel :: Double
        ,age       :: Int
        ,aim       :: Mode
        ,path      :: [Direction]
        }
        deriving(Eq)

instance Show Ant where
         show (Ant {pherLevel = i} ) = "Ant: " ++ show i



oppDir x | x == North = South
         | x == South = North
         | x == East  = West
         | x == West  = East


