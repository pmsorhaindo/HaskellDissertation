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
type GraphPWTuple = (Graph, Vertex -> (GraphPTuple, Int, [Int]), Int -> Maybe Vertex)

--Test-Framework .... to automate the testing

--Build a new Graph 6 and 36 need replacing with a variable graph size and graph size^2
-- let a = graphFromEdges $ zip3 [1..36] (keyList 6) (adjListForNewGraph 6)

-- | Just 0's in a graph of size supplied
testBuildEmptyWorld size = graphFromEdges $ zip3 (replicate (size^2) 0) (keyList size) (adjListForNewGraph size)

-- | Globals\
a_ = graphTuple edgesForTestAGraph
b_ = graphTuple edgesForTestPGraph

worldWidth = 3 -- this calls for the generation of 9 Ant Worlds and Pheremone Maps
antWorld = graphFromEdges $ zip3 (listOfAntQuadrants) (keyList worldWidth) (adjListForNewGraph worldWidth)

antWorld' = graphFromEdges $ zip3 (listOfAntQuadrants') (keyList worldWidth) (adjListForNewGraph worldWidth)
        where listOfAntQuadrants' = [newAQuad',newAQuad 3,newAQuad 3,newAQuad 3,newAQuad',newAQuad 3,newAQuad 3,newAQuad 3,newAQuad']

pherWorld = graphFromEdges $ zip3 (listOfPherQuadrants) (keyList worldWidth) (adjListForNewGraph worldWidth)

-- | Generate empty ant world based on the size (worldWidth) provided as a prameter
newAWorld worldWidth quadWidth = graphFromEdges $ zip3 (replicate (worldWidth^2) (newAQuad quadWidth)) (keyList worldWidth) (adjListForNewGraph worldWidth)

-- | Generate empty pheremone world based on the size (worldWidth) provided as a prameter
newPWorld worldWidth quadWidth = graphFromEdges $ zip3 (replicate (worldWidth^2) (newPQuad quadWidth)) (keyList worldWidth) (adjListForNewGraph worldWidth)


updatedAWorld newAQuads = graphFromEdges $ zip3 (newAQuads) (keyList worldWidth) (adjListForNewGraph worldWidth)

--listOfAntQuadrants = replicate (worldWidth^2) emptyAntQuadrant
listOfAntQuadrants = replicate (worldWidth^2) a''

listOfPherQuadrants = replicate (worldWidth^2) emptyPherQuadrant

stitchUp quad1 quad2 = undefined

showAntQuad nd world = brokenUpGraph $ fstTrip ((sndTrip world) nd)

getAntQuad :: Int -> GraphAWTuple -> GraphATuple
getAntQuad nd world = fstTrip ((sndTrip world) nd)

getPherQuad :: Int -> GraphPWTuple -> GraphPTuple
getPherQuad nd world = fstTrip ((sndTrip world) nd)

-- | gets the edge of the Graph at nd in the world  ::ghci getAQuadEdge 1 West
getAQuadEdge nd edgDir = getAEdge (getAntQuad nd antWorld) edgDir
getPQuadEdge nd edgDir = getAEdge (getPherQuad nd pherWorld) edgDir

--1|2|3|4
--5|6|7|8
--9|A|B|C
--D|E|F|G

--Get All the stitchupable edges 
stitchEdges :: GraphAWTuple -> [(Int, Int)]
stitchEdges world = nubBy (\x y -> x == y || swap x == y) (edges $ fstTrip $ world)
-- nubBy allows me to remove duplicates from a list like nub but with nubBy I can describe how duplicates 

dirsNeeded :: (Int, Int) -> ((Int, Direction), (Int, Direction)) -- NEEDED TO GENERATE THE quadPairs used in stitchUpEdge
dirsNeeded quadTuple
   | snd quadTuple == fst quadTuple+1 = ((fst quadTuple,West),(snd quadTuple,East))
   | fst quadTuple == snd quadTuple+1 = ((fst quadTuple,East),(snd quadTuple,West))
   | fst quadTuple > snd quadTuple    = ((fst quadTuple,South),(snd quadTuple,North))
   | fst quadTuple < snd quadTuple    = ((fst quadTuple,North),(snd quadTuple,South))


genQuadPairs :: [(Int, Int)] -> [((Int, Direction), (Int, Direction))]
genQuadPairs pairs = map dirsNeeded pairs

--stitchUpEdges :: GraphAWTuple -> GraphAWTuple NOT NEEDED?!
--stitchUpEdges world = undefined --stitchEdges edges $ fstTrip world -- more serially stuffs

--stitchUpEdge :: GraphAWTuple -> GraphPWTuple -> ((Int,Direction),(Int,Direction)) -> Int -> GraphAWTuple
stitchUpEdge antWorld pherWorld quadPair qsiz = do

                let quadSize = qsiz                

                let rel = ((snd $ fst quadPair), (snd $ snd quadPair))
                
                let ags = ((getAntQuad (fst $ fst quadPair) antWorld), (getAntQuad (fst$ snd quadPair) antWorld)) -- ant graph pair
                let pgs = ((getPherQuad (fst $ fst quadPair) pherWorld), (getPherQuad (fst $ snd quadPair) pherWorld)) -- pher graph pair

                let aep = ((getAEdge (fst ags) (snd $ fst quadPair)) , (getAEdge (snd ags) (snd $ snd quadPair))) --ant edge pair
                let pep = ((getPEdge (fst pgs) (snd $ fst quadPair)) , (getPEdge (snd pgs) (snd $ snd quadPair))) -- pher edge pair

                let noProcLines = ([],[])

                let stitchable = StitchableQuads quadSize rel ags pgs aep pep noProcLines

                stitchable
                
                
--based off listOfNodesWithAntsIn
edgePointsWithAntsIn :: [(Maybe Ant, Int)] -> [(Maybe Ant,Int)]
edgePointsWithAntsIn [] = []
edgePointsWithAntsIn ((ant,nd):xs)
        | not $ isNothing ant  = (ant,nd) : edgePointsWithAntsIn xs
        | otherwise = edgePointsWithAntsIn xs
                       
--increment vert tuple to get nodes
-- get the direction needed from each node.
-- get the edges for those directions from the nodes
-- process ants at Edges

-- Other methods
-- flags
-- Ant pools


processQuadrants :: GraphAWTuple -> GraphPWTuple -> GraphAWTuple
processQuadrants = undefined -- TODO parallel stuff


processWorld :: GraphAWTuple -> GraphAWTuple
processWorld = undefined -- TODO the big loop




