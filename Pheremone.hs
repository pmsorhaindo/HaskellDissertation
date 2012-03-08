--PheremoneWorld
module Pheremone where
import World
import Data.Graph

emptyPherWorld size = graphFromEdges $ zip3 (replicate (size^2) 0) (keyList size) (adjListForNewGraph size)


