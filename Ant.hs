module Ant where
--Ant

import SimDefine

data Ant = Ant Location Direction
        deriving(Show)

updatePosition :: Location -> Direction -> Location
updatePosition (Location x y) North = Location x (y+1)
updatePosition (Location x y) South = Location x (y-1)
updatePosition (Location x y) East  = Location (x+1) y
updatePosition (Location x y) West  = Location (x-1) y

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

moveAnt :: Ant -> Ant
moveAnt (Ant (Location x y) d) = Ant l d
        where l = updatePosition (Location x y) d

antTurnRight :: Ant -> Ant
antTurnRight (Ant l dir) = Ant l newd
        where newd = turnRight dir

antTurnLeft :: Ant -> Ant
antTurnLeft (Ant l dir) = Ant l newd
        where newd = turnLeft dir
