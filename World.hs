module World where
import Data.Array
import Data.Graph
import Data.List (nubBy,(\\))
import Data.Maybe (fromJust, isNothing)
import Data.Tuple (swap)
import Test.QuickCheck
import Test.HUnit

-- Personal Imports
import AntRepresent
import Quadrant
import GraphOps
import SimDefine
import QuadStitching

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


antWorld_ = graphFromEdges $ zip3 (listOfAntQuadrants') (keyList worldWidth) (adjListForNewGraph worldWidth)
        where listOfAntQuadrants' = [a1',a2',a3',a4']

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


inTupleList ((x,y):xs) xs1 = do
        let deps = concatMap (\(a,b) -> a : b : []) xs1
        if x `elem` deps || y `elem` deps
                then inTupleList xs xs1
                else inTupleList xs ((x,y):xs1)

inTupleList ((x,y):xs) []= inTupleList xs ((x,y):[])
inTupleList [] a = a


-- *Main> stitchEdges antWorld
-- [(0,3),(0,1),(1,4),(1,2),(2,5),(3,6),(3,4),(4,7),(4,5),(5,8),(6,7),(7,8)]

getBatch stitchies em batches = do
        let x = inTupleList stitchies em
        if x == []
                then batches
                else do
                        let y = inTupleList stitchies em
                        let s = (\\) stitchies y                     
                        getBatch  s []  (y:batches)
                        
-- not good but close


{- *Main> inTupleList it []
[(6,7),(2,5),(1,4),(0,3)]
*Main> let wamba = stitchEdges antWorld
*Main> let err =  inTupleList wamba []
*Main> err
[(6,7),(2,5),(1,4),(0,3)]
*Main> :m + Data.List
*Main Data.List> wamba \\ err
[(0,1),(1,2),(3,6),(3,4),(4,7),(4,5),(5,8),(7,8)]
*Main Data.List> err
[(6,7),(2,5),(1,4),(0,3)]
*Main Data.List> wamba
[(0,3),(0,1),(1,4),(1,2),(2,5),(3,6),(3,4),(4,7),(4,5),(5,8),(6,7),(7,8)]
*Main Data.List> :t (\\)
(\\) :: Eq a => [a] -> [a] -> [a]
*Main Data.List> err
[(6,7),(2,5),(1,4),(0,3)]
*Main Data.List> wamba
[(0,3),(0,1),(1,4),(1,2),(2,5),(3,6),(3,4),(4,7),(4,5),(5,8),(6,7),(7,8)]
*Main Data.List> wamba \\ err
[(0,1),(1,2),(3,6),(3,4),(4,7),(4,5),(5,8),(7,8)]
*Main Data.List> (\\)wamba err
[(0,1),(1,2),(3,6),(3,4),(4,7),(4,5),(5,8),(7,8)]
*Main Data.List> (\\) wamba err
[(0,1),(1,2),(3,6),(3,4),(4,7),(4,5),(5,8),(7,8)]
*Main Data.List> wamba
[(0,3),(0,1),(1,4),(1,2),(2,5),(3,6),(3,4),(4,7),(4,5),(5,8),(6,7),(7,8)]
*Main Data.List> (\\) wamba err
[(0,1),(1,2),(3,6),(3,4),(4,7),(4,5),(5,8),(7,8)]
*Main Data.List> let err2 =  inTupleList it []
*Main Data.List> err2
[(5,8),(4,7),(3,6),(0,1)]
*Main Data.List> (\\) wamba err 
[(0,1),(1,2),(3,6),(3,4),(4,7),(4,5),(5,8),(7,8)]
*Main Data.List> let rem1 = (\\) wamba err 
*Main Data.List> rem1 \\ err2
[(1,2),(3,4),(4,5),(7,8)]
*Main Data.List> inTupleList it []
[(7,8),(3,4),(1,2)] -}


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




