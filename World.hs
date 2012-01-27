module World where
import Data.Array
import Data.Graph
import Data.List (nubBy)
import Data.Maybe (fromJust, isNothing)
import Data.Tuple (swap)
import Test.QuickCheck
import Test.HUnit
import AntRepresent
import Quadrant
import GraphOps

type GraphAWTuple = (Graph, Vertex -> (GraphATuple, Int, [Int]), Int -> Maybe Vertex)
--Test-Framework .... to automate the testing

--Build a new Graph 6 and 36 need replacing with a variable graph size and graph size^2
-- let a = graphFromEdges $ zip3 [1..36] (keyList 6) (adjListForNewGraph 6)
buildEmptyWorld size = graphFromEdges $ zip3 (replicate (size^2) 0) (keyList size) (adjListForNewGraph size)

-- | Globals\
a_ = graphTuple edgesForTestAGraph
b_ = graphTuple edgesForTestPGraph

worldWidth = 3 -- this calls for the generation of 9 Ant Worlds and Pheremone Maps
antWorld = graphFromEdges $ zip3 (listOfAntQuadrants) (keyList worldWidth) (adjListForNewGraph worldWidth)
pherWorld = graphFromEdges $ zip3 (listOfPherQuadrants) (keyList worldWidth) (adjListForNewGraph worldWidth)

listOfAntQuadrants = replicate (worldWidth^2) emptyAntQuadrant

listOfPherQuadrants = replicate (worldWidth^2) emptyPherQuadrant

stitchUp quad1 quad2 = undefined

showAntQuad nd world = brokenUpGraph $ fstTrip ((sndTrip world) nd)

getAntQuad :: Int -> GraphAWTuple -> GraphATuple
getAntQuad nd world = fstTrip ((sndTrip world) nd)

-- | gets the edge of the Graph at nd in the world  ::ghci getAQuadEdge 1 West|8
getAQuadEdge nd edgDir = getAEdge (getAntQuad nd antWorld) edgDir



--1|2|3|4
--5|6|7|8
--9|A|B|C
--D|E|F|G

--Get All the stitchupable edges 
stitchEdges :: [(Int, Int)] -> [(Int, Int)]
stitchEdges xs = nubBy (\x y -> x == y || swap x == y) xs
-- nubBy allows me to remove duplicates from a list like nub but with nubBy I can describe how duplicates 


processWorld :: GraphAWTuple -> GraphAWTuple
processWorld = undefined -- TODO the big loop

stitchUpEdges :: GraphAWTuple -> GraphAWTuple
stitchUpEdges world = --stitchEdges edges $ fstTrip world -- more serially stuffs

stitchUpEdge :: GraphAWTuple -> (Int,Int) -> GraphAWTuple
stitchUpEdges world = undefined
--increment vert tuple to get nodes
-- get the direction needed from each node.
-- get the edges for those directions from the nodes
-- process ants at Edges

--Other methods
-- flags
-- Ant pools

processQuadrants :: GraphAWTuple -> GraphAWTuple
processQuadrants = undefined -- TODO parallel stuffs


--for each element of the list fold delete the swapped element of list over list. if reach the end of list return list. 





