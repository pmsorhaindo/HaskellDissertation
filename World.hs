module World where
import Data.Array
import Data.Graph
import Data.List (nubBy,(\\),sortBy,union)
import Data.Maybe (fromJust,isJust,isNothing)
import Data.Bool.HT
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
type GraphFWTuple = (Graph, Vertex -> (GraphFTuple, Int, [Int]), Int -> Maybe Vertex)
type GraphNWTuple = (Graph, Vertex -> (GraphNTuple, Int, [Int]), Int -> Maybe Vertex)

--Build a new Graph 6 and 36 need replacing with a variable graph size and graph size^2
-- let a = graphFromEdges $ zip3 [1..36] (keyList 6) (adjListForNewGraph 6)

-- | Just 0's in a graph of size supplied
testBuildEmptyWorld size = graphFromEdges $ zip3 (replicate (size^2) 0) (keyList size) (adjListForNewGraph size)

-- | Globals\
a_ = graphTuple edgesForTestAGraph
b_ = graphTuple edgesForTestPGraph

-- Global pre predeclared Worlds for testing. --

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

-- | Generate empty food world based on the size (worldWidth) provided as a prameter
newFWorld worldWidth quadWidth = graphFromEdges $ zip3 (replicate (worldWidth^2) (newFQuad quadWidth)) (keyList worldWidth) (adjListForNewGraph worldWidth)

-- | Generate empty nest world based on the size (worldWidth) provided as a prameter
newNWorld worldWidth quadWidth = graphFromEdges $ zip3 (replicate (worldWidth^2) (newNQuad quadWidth)) (keyList worldWidth) (adjListForNewGraph worldWidth)

updatedAWorld newAQuads = graphFromEdges $ zip3 (newAQuads) (keyList worldWidth) (adjListForNewGraph worldWidth)

--listOfAntQuadrants = replicate (worldWidth^2) emptyAntQuadrant
listOfAntQuadrants = replicate (worldWidth^2) a''


listOfPherQuadrants = replicate (worldWidth^2) emptyPherQuadrant

-- | This prints out the details of an Ant Quadrant to the terminal given its node in the antWorld Graph.
showAntQuad nd world = brokenUpGraph $ fstTrip ((sndTrip world) nd)

-- | This function pulls a quadrant out of an ant world graph when given its index.
getAntQuad :: Int -> GraphAWTuple -> GraphATuple
getAntQuad nd world = fstTrip ((sndTrip world) nd)

-- | This function pulls a quadrant out of a pheremone world graph given its index.
getPherQuad :: Int -> GraphPWTuple -> GraphPTuple
getPherQuad nd world = fstTrip ((sndTrip world) nd)

-- | This function returns the edge of the Graph at a particular node in the world  ::ghci getAQuadEdge 1 West
getAQuadEdge nd edgDir = getAEdge (getAntQuad nd antWorld) edgDir
getPQuadEdge nd edgDir = getAEdge (getPherQuad nd pherWorld) edgDir

--1|2|3|4
--5|6|7|8
--9|A|B|C
--D|E|F|G

-- | Get All the stitchupable edges. This generates a list of all the quadrant edges as pairs of the quadrant indices into the world graph that 
--   form them.
stitchEdges :: GraphAWTuple -> [(Int, Int)]
stitchEdges world = nubBy (\x y -> x == y || swap x == y) (edges $ fstTrip $ world)
-- nubBy allows me to remove duplicates from a list like nub but with nubBy I can describe how duplicates

-- *Main> stitchEdges antWorld
-- [(0,3),(0,1),(1,4),(1,2),(2,5),(3,6),(3,4),(4,7),(4,5),(5,8),(6,7),(7,8)]

-- | Gets the batches for parallel stitching in order.
getOrderedBatch a = reverse $ sortBy (\x y -> length y `compare` length x) (getBatch a [] [])

-- | Generates the batches of computation for parallel stitching, this produces a list of lists, where each list is a set of stitching quads
--   which can be stitched in parallel without loosing the worlds coherence.
getBatch stitchies emptyList batches = do
        let x = inTupleList stitchies emptyList
        if x == []
                then batches
                else do
                        let y = inTupleList stitchies emptyList
                        let s = (\\) stitchies y                     
                        getBatch  s []  (y:batches)

-- | Helper function for getBatch
inTupleList ((x,y):xs) xs1 = do
        let deps = concatMap (\(a,b) -> a : b : []) xs1
        if x `elem` deps || y `elem` deps
                then inTupleList xs xs1
                else inTupleList xs ((x,y):xs1)

--inTupleList (a:xs) []= inTupleList xs (a:[])
inTupleList [] a = a

                        
-- | Produces the intDirection tuple needed for stitchable Quad Generation
genQuadPairs :: [(Int, Int)] -> [((Int, Direction), (Int, Direction))]
genQuadPairs pairs = map dirsNeeded pairs

-- | Helper function for genQuad Pairs
dirsNeeded :: (Int, Int) -> ((Int, Direction), (Int, Direction)) -- NEEDED TO GENERATE THE quadPairs used in stitchUpEdge
dirsNeeded quadTuple
   | snd quadTuple == fst quadTuple+1 = ((fst quadTuple,West),(snd quadTuple,East))
   | fst quadTuple == snd quadTuple+1 = ((fst quadTuple,East),(snd quadTuple,West))
   | fst quadTuple > snd quadTuple    = ((fst quadTuple,South),(snd quadTuple,North))
   | fst quadTuple < snd quadTuple    = ((fst quadTuple,North),(snd quadTuple,South))


--stitchUpEdges :: GraphAWTuple -> GraphAWTuple NOT NEEDED?!
--stitchUpEdges world = undefined --stitchEdges edges $ fstTrip world -- more serially stuffs

--stitchUpEdge :: GraphAWTuple -> GraphPWTuple -> ((Int,Direction),(Int,Direction)) -> Int -> GraphAWTuple
-- | stitchUpEdge 
stitchUpEdge antWorld pherWorld qsiz quadPair = do

                let quadSize = qsiz                

                let rel = ((snd $ fst quadPair), (snd $ snd quadPair))
                
                let ags = ((getAntQuad (fst $ fst quadPair) antWorld), (getAntQuad (fst$ snd quadPair) antWorld)) -- ant graph pair
                let pgs = ((getPherQuad (fst $ fst quadPair) pherWorld), (getPherQuad (fst $ snd quadPair) pherWorld)) -- pher graph pair

                let aep = ((getAEdge (fst ags) (snd $ fst quadPair)) , (getAEdge (snd ags) (snd $ snd quadPair))) --ant edge pair
                let pep = ((getPEdge (fst pgs) (snd $ fst quadPair)) , (getPEdge (snd pgs) (snd $ snd quadPair))) -- pher edge pair

                let noProcLines = ([],[])

                let stitchable = StitchableQuads quadSize rel ags pgs aep pep noProcLines

                stitchable
                
                 
-- | This function is based off listOfNodesWithAntsIn with pattern matching it loops through a list of ants and their vertices on a given edge --   and returns a list of maybe ants where all values are Just ant, Nothings are disregarded.
edgePointsWithAntsIn :: [(Maybe Ant, Int)] -> [(Maybe Ant,Int)]
edgePointsWithAntsIn [] = []
edgePointsWithAntsIn ((ant,nd):xs)
        | not $ isNothing ant  = (ant,nd) : edgePointsWithAntsIn xs
        | otherwise = edgePointsWithAntsIn xs
                       
-- increment vert tuple to get nodes
-- get the direction needed from each node.
-- get the edges for those directions from the nodes
-- process ants at Edges

-- Other methods
-- flags
-- Ant pools

-- |
--processWorld :: GraphAWTuple -> GraphPWTuple -> GraphAWTuple
processWorld x y = do
        let siz = truncate $ sqrt $fromIntegral $length $brokenUpGraph $ fstTrip ((sndTrip x) (0))         
        let edgePairs = stitchEdges x
        let parallelBatches = getOrderedBatch edgePairs
        let includeDirs = recurseOnBatches parallelBatches
        let stis = stitchUpEdge x y siz -- partially applied function ready for mass creation of stitchableQuads
        let batchResult = batchStitching stis includeDirs x y siz
        let worldRes = fst batchResult
        let noProcs = snd batchResult
        worldRes


-- | The takes pairs of quadrant vertexes into a world graph and returns the relation to each other as a tuple in terms of sides touching.
--   for example the top two quadrants indexed 1 and to would be input as (1,2) and would be returned as ((1,West),(2,East)) the second element
--   in each tuple being the direction for the edge which touches the other quadrant which is indexed.
--   The seperate lists whould be sets of quadrant pairs which can be processed in parallel.
recurseOnBatches :: [[(Int,Int)]]                   -- ^
        -> [[((Int, Direction), (Int, Direction))]] -- ^
recurseOnBatches x =  map genQuadPairs x

-- |
--batchStitching :: (((Int, Direction), (Int, Direction)) -> StitchableQuads) -> [[((Int, Direction), (Int, Direction))]] ->
batchStitching f list@(x:xs) aworld pworld siz = do
        let a = map f x
        let b = map (procEdgeAntAtNode 0) a -- parallelizable
        let noProcs = map noProcList b
        let noProcTable = zipNoProcs x noProcs [] siz
        let newQuads = map antGraphs b        
        let aworld' =  updateAntWorld x newQuads aworld []
        let f' =  stitchUpEdge aworld' pworld siz --updated partially applied stitchies
        batchStitching' f' xs aworld' pworld siz noProcTable


-- |

batchStitching' f list@(x:xs) aworld pworld siz noProcTable = do 
        let a = map f x
        let b = map (procEdgeAntAtNode 0) a -- parallelizable
        let noProcs = map noProcList b
        let noProcTable = zipNoProcs x noProcs noProcTable siz
        let newQuads = map antGraphs b
        let aworld' =  updateAntWorld x newQuads aworld []
        batchStitching' f xs aworld' pworld siz noProcTable

batchStitching' f [] world pworld siz noProcTable = (world,noProcTable)


-- |

updateAntWorld ((x1,x2):xs) ((y1,y2):ys) world passedListQuads = updateAntWorld xs ys world ((fst x1,y1):(fst x2,y2):passedListQuads)

updateAntWorld [] _ world passedList = do
        let siz = truncate $ sqrt $fromIntegral $length $ brokenUpGraph world
        if length passedList == siz^2
                then simpleAGraphUpdate passedList siz
                else complexAGraphUpdate world passedList siz

-- |

simpleAGraphUpdate passedList siz = do       
        let orderedList = sortBy (\x y -> fst y `compare` fst x) passedList
        let quads = map snd orderedList
        let update = graphFromEdges $ zip3 (quads) (keyList siz) (adjListForNewGraph siz)
        update

-- |

complexAGraphUpdate world passedList siz = do
        patchMissing world passedList 0 siz []
        
-- |
patchMissing world passedList inc siz qList = do
        let potentialQuad = lookup inc passedList
        let qList' = select [] $ 
                (inc> ((siz^2)-1)        , qList)
                :(isNothing potentialQuad, qList ++ [(getAntQuad inc world)])
                :(isJust potentialQuad   , qList ++ [(fromJust $lookup inc passedList)])
                :[]
        if length qList == length qList'
                then graphFromEdges $ zip3 (qList) (keyList siz) (adjListForNewGraph siz)
                else patchMissing world passedList (inc+1) siz qList'
        

-- | Creates and maintains a table for the no process vertices for all Quadrants.
zipNoProcs :: [((Int, Direction), (Int, Direction))] -- ^
     -> [([Int], [Int])]                             -- ^
     -> [[Int]]                                      -- ^
     -> Int                                          -- ^
     -> [[Int]]                                      -- ^ Returns a No Process List Table
zipNoProcs labels@(((x1,_),(x2,_)):xs) noProcs@((y1,y2):ys) [] siz = do
        let a = replicate siz []
        let b = updateNoProcList x1 y1 a
        let c = updateNoProcList x2 y2 b
        c

zipNoProcs labels@(((x1,_),(x2,_)):xs) noProcs@((y1,y2):ys) npList@(l:ls) siz = do
        let a = updateNoProcList x1 y1 npList
        let b = updateNoProcList x1 y1 a
        b

-- |
updateNoProcList :: Int         -- ^
        -> [Int]                -- ^
        -> [[Int]]              -- ^
        -> [[Int]]              -- ^
updateNoProcList quadrant addition currentNoProcList = do
        let a = splitAt quadrant currentNoProcList
        let y = fst a ++ [union addition (head $ snd a)] ++ (tail $ snd a)
        y

-- | 
addAntToWorld :: Ant                    -- ^ The desired ant to be added to the simulation world.
        -> Location                     -- ^ The location that the simulation wants to place the ant at.
        -> GraphAWTuple                 -- ^ The current Ant world graph.
        -> Either String GraphAWTuple   -- ^ Either a returned updated ant graph or an error message if the coordinates do not exist within the world.
addAntToWorld a loc awgraph = do
        let wsiz = truncate $ sqrt $fromIntegral $length $brokenUpGraph awgraph
        let qsiz = truncate $ sqrt $fromIntegral $length $brokenUpGraph $ fstTrip ((sndTrip awgraph) (0))
        let siz = wsiz * qsiz
        let x = xpos loc
        let y = ypos loc
        if (x <= siz && y <= siz)
                then addAntToWorld' a loc awgraph wsiz qsiz
                else Left "Ant is not of this world."


-- | This is a helper funcion to addAntToWorld and converts the world coordinates to coordinates for a specific quad
--   it also extracts the necessary quadrant for updating.
addAntToWorld' :: Ant                   -- ^ The ant to be added to the simulation
        -> Location                     -- ^ The requested location for the ant within the simulation world
        -> GraphAWTuple                 -- ^ The ant world graph to be edited.
        -> Int                          -- ^ The size of ant world graph in terms of quadrants across.
        -> Int                          -- ^ The size of each quadrant in terms of nodes across.
        -> Either String GraphAWTuple   -- ^ The result, either an error string if an ant already exists in the desired location or the edited ant world Graph.
addAntToWorld' a loc awgraph wsiz qsiz = do
        let wnd = ((xpos loc -1) `div` qsiz) + (((ypos loc -1) `div` qsiz) * wsiz)
        let aquad = fstTrip $ (sndTrip awgraph) (wnd)
        let qnd = 1 + ((xpos loc - 1) `mod` qsiz + (((ypos loc -1) `mod` qsiz) * wsiz))
        if isAntAtNode aquad qnd       
                then Left "Ant already here."
                else addAntToWorld'' wnd awgraph (addExistingAnt aquad qnd (Just a))


-- | testing wnode function on a fixed 3x3 quad 9 quadrant world. World coordinate (1-9,1-9)
wnodeTest :: Int        -- ^ The given x world coordinate
        -> Int          -- ^ The given y world coordinate
        -> Int          -- ^ The resulting quadrant the coordinate should lie in.
wnodeTest x y = ((x -1) `div` 3) + (((y -1) `div` 3) * 3)

-- | testing qnode function on a fixed 3x3 quad 9 quadrant world. World coordinate (1-9,1-9)
qnodeTest :: Int        -- ^ The given x world coordinate
        -> Int          -- ^ The given y world coordinate
        -> Int          -- ^ The resulting node within the quadrant the world coordinate identifies.
qnodeTest x y = 1 + ((x - 1) `mod` 3 + (((y -1) `mod` 3) * 3))

-- | This function updates the specific quadrant the ant is to be added to then updates the world replacing only the updated quadrant in the world Graph.
addAntToWorld'' :: Int                  -- ^ The quadrant to be changed's node position within the ant world graph.
        -> GraphAWTuple                 -- ^ The ant world Graph to be changed.
        -> GraphATuple                  -- ^ The ant edited ant quandrant
        -> Either String GraphAWTuple   -- ^ The returned ant world graph.
addAntToWorld'' wnd awgraph newQuad = Right (graphFromEdges $ zip3 nds nverts nadjs)
        where
               wid = truncate $ sqrt $fromIntegral $length $brokenUpGraph awgraph
               nds = (replace newQuad wnd (map fstTrip $ brokenUpGraph awgraph))
               nverts = (keyList wid)
               nadjs = adjListForNewGraph (wid)


-- | This function replaces a Quadrant within a list of Quadrants.
{-replace :: GraphATuple          -- ^ The edited ant quadrant.
        -> Int                  -- ^ The index of the graph that needs to  be replaced
        -> [GraphATuple]        -- ^ The list of ant quadrants
        -> [GraphATuple]        -- ^ The resulting edited list.-}
replace item index list = a ++ item : b
        where
                a = take (index) list
                b = drop index list

-- | Extracts the a given value out of the Either Monadic Type
extractAntPlaceEither (Right a) _ 0 rs _ =  (a,rs)
extractAntPlaceEither (Right a) aw antNum ((x,y):rs) aidST =  populateAntWorld a (antNum-1) (rs) aidST --aidST Ant Id from StateM
extractAntPlaceEither (Left b) aw antNum ((x,y):rs) aidST = populateAntWorld aw (antNum) rs aidST
extractAntPlaceEither (Right a) aw antNum [] aidST =  (a,[])
extractAntPlaceEither (Left b) aw antNum [] aidST = (aw,[])

-- | Given a list of world coordinates this function populates an AntWorld with the given amount of ants.
populateAntWorld :: GraphAWTuple        -- ^ The current ant world graph.
        -> Int                          -- ^ The number of ants to be added to the simulation.
        -> [(Int, Int)]                 -- ^ Takes a list of coordinates that ants are to be placed at.
        -> t1                           -- ^ Ant Id -- TODO State Monad.
        -> (GraphAWTuple,[(Int,Int)])   -- ^ The resulting ant updated ant world graph.
populateAntWorld aw antNum ((x,y):rs) aidST  = do
        let checkw = addAntToWorld (blankAnt 1) (Location x y) aw
        extractAntPlaceEither checkw aw (antNum) rs aidST



-- | Given a list of world coordinates this function initialzes a nest world graph with a starting nest location.
populateNestWorld :: GraphNWTuple       -- ^ The current nest world graph.
        -> [(Int, Int)]                 -- ^ Takes a list of coordinates that the nests starting location could be placed at.
        -> (GraphNWTuple,(Int,Int))     -- ^ The resulting updated nest world graph.
populateNestWorld nw ((x,y):rs) = do
        let wsiz = truncate $ sqrt $fromIntegral $length $brokenUpGraph nw
        let qsiz = truncate $ sqrt $fromIntegral $length $brokenUpGraph $ fstTrip ((sndTrip nw) (0))
        let siz = wsiz * qsiz
        if (x <= siz && y <= siz)
                then placeNestStart x y nw wsiz qsiz
                else placeNestStart 1 1 nw wsiz qsiz

placeNestStart x y nwgraph wsiz qsiz = do
        let wnd = ((x -1) `div` qsiz) + (((y -1) `div` qsiz) * wsiz)
        let nquad = fstTrip $ (sndTrip nwgraph) (wnd)
        let qnd = 1 + ((x - 1) `mod` qsiz + (((y -1) `mod` qsiz) * wsiz))
        placeNestStart' wnd nwgraph (buildNest nquad qnd) (x,y)


placeNestStart' wnd nwgraph newQuad coords = ((graphFromEdges $ zip3 nds nverts nadjs), coords)
        where
               wid = truncate $ sqrt $fromIntegral $length $brokenUpGraph nwgraph
               nds = (replace newQuad wnd (map fstTrip $ brokenUpGraph nwgraph))
               nverts = (keyList wid)
               nadjs = adjListForNewGraph (wid)


populateFoodWorld fw foodNum ((x,y):rs) = do
        let wsiz = truncate $ sqrt $fromIntegral $length $brokenUpGraph nw
        let qsiz = truncate $ sqrt $fromIntegral $length $brokenUpGraph $ fstTrip ((sndTrip nw) (0))
        let siz = wsiz * qsiz
        if (x <= siz && y <= siz)
                then placeFoodSource x y nw wsiz qsiz
                else Left "Food must be placed within the world"



