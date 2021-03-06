module AntRepresent where
import SimDefine


data Mode = Explore { dir :: Direction, depth :: Int}
        | TrackFood {latestPher :: [Int]}
        | Return {steps :: Int} --
        | TrackNest {latestPhers :: [Int]}
        | Wander -- possibly add more behaviour types here
        deriving (Eq)

data Ant = Ant {
         antId     :: Int
        ,antDir    :: Direction
        ,pherLevel :: Double
        ,age       :: Int
        ,aim       :: Mode
        ,path      :: [Direction]
        -- possibly add more ant attributes here
        -- memory :: Int number of steps in the past the ant remembers used for return.
        }
        deriving(Eq)

instance Show Ant where
         show (Ant {pherLevel = i} ) = "Ant: " ++ show i

blankAnt x = Ant x North 0.0 0 Wander []



oppDir x | x == North = South
         | x == South = North
         | x == East  = West
         | x == West  = East


