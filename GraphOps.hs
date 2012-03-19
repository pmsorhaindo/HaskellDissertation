module GraphOps where

import Data.Array
import Data.Graph
import Data.Maybe (fromJust, isNothing)
import Data.List (maximumBy, elem)
import Test.QuickCheck
import Test.HUnit

type Size = Int
type Point = Int

-- TODO adjKeyList a = [genUp,genDown,genLeft,genRight] --need a list of these lists for each key in keyList
-- secure these functions with better testing on numbers added AdjRight still allows ridiculos -ve numbers
-- left v-1 (not avaiable if v(modir) size =1) TESTED
-- | This function gets the adjacent node value given the Quadrant width


-- | Utility function for deconstructing 3 tuples with pattern matching (first value of the tuple)
fstTrip (x,y,z) = x
-- | Utility function for deconstructing 3 tuples with pattern matching (second value of the tuple)
sndTrip (x,y,z) = y
-- | Utility function for deconstructing 3 tuples with pattern matching (third value of the tuple)
trdTrip (x,y,z) = z

-- | Pulling apart the Graph 3 tuple from a generated Graph (This returns the Graph itself)
graph edgs         = fstTrip $ graphFromEdges edgs

-- | Pulling apart the Graph 3 tuple from a generated Graph (This returns a function that returns the node at a given vertex in the generated Graph)
graphfunc edgs     = sndTrip $ graphFromEdges edgs

-- | Pulling apart the Graph 3 tuple from a generated Graph (Returns a function that when given a Vertex tells you if the Graph contains it.)
graphfuncVert edgs = trdTrip $ graphFromEdges edgs


--Adjacency List Automation
-- | Left v-1 (not available if v(mod)size = 1) TESTED
getAdjLeft:: Size->[Int]->Point->[Int]
getAdjLeft size currentAdj pos
                                | (pos <= 0 || pos > size^2 ) || pos`mod`size == 1 = currentAdj
                                | otherwise         = (pos-1):currentAdj

-- | Right v+1 (not available if v(mod)size = 0 ((not including 0 use range))) TESTED
getAdjRight:: Size->[Int]->Point->[Int]
getAdjRight size currentAdj pos
                                | (pos <= 0 || pos > size^2 ) || pos`mod`size == 0 = currentAdj
                                | otherwise                     = (pos+1):currentAdj

-- | Down v+size (not available if v>(size^2-size) TESTED
getAdjDown:: Size->[Int]->Point->[Int]
getAdjDown size currentAdj pos
                                | pos > (size^2-size) || pos <= 0 = currentAdj
                                | otherwise                       = (pos+size):currentAdj

-- | Up v-size (not available if v<=size) TESTED
getAdjUp:: Size->[Int]->Point->[Int]
getAdjUp size currentAdj pos
                                | (pos-size) <= 0 || pos > size^2 = currentAdj
                                | otherwise                       = (pos-size):currentAdj

-- | Helper function for adjListForNewGraph
adjListForVertex :: Size -> Point -> [Int]
adjListForVertex size pos = concat $ map ($pos) [(getAdjUp size []),(getAdjLeft size []),(getAdjDown size []),(getAdjRight size [])]

-- | Generates an adjacency for graph generation purposes. The Adjacency list will be for a graph in the form of a lattice of the size provided
adjListForNewGraph :: Size -> [[Int]]
adjListForNewGraph size = map (adjListForVertex size) [1..size^2]


-- Other useful Graph functions
-- | List of Nodes -- start list of empty nodes size od size^2
keyList :: Int -> [Int]
keyList size = [1..size^2]

-- | returns the node at vertex y from given edge structure x
nodeAtVert :: (Ord key) => [(node, key, [key])] -> Vertex -> node
nodeAtVert x y = fstTrip $ graphfunc x y

-- | for Edge Structure x, map the find nodeAtVert function across all the nodes in the graph.
listOfNodes :: Ord key => [(node, key, [key])] -> [node]
listOfNodes x = map (nodeAtVert x) [0..(snd $ bounds (graph x))]



-- | Tests
tests = TestList $ map TestCase
        [assertEqual "" 1 1
        ]

newtype IntSmall = IntSmall Int
        deriving (Show)

instance Arbitrary IntSmall where
        arbitrary = do
        i <- choose(0,1000) -- or some low value to stop things getting too big
        return $ IntSmall i

newtype NodeList = NodeList [(Int,Vertex,[Vertex])]
        deriving(Show)

instance Arbitrary NodeList where
        arbitrary = do
        siz <- choose(0,1000)
        nodes <- listOf (choose (0,1))
        let adjs = adjListForNewGraph siz
        let ndList = zip3 (take siz nodes) (keyList siz) adjs
        return $ NodeList ndList


prop_keyList_size :: IntSmall -> Bool
prop_keyList_size (IntSmall x) = (length (keyList x) == x^2)

prop_adjListForNewGraph_size :: IntSmall -> Bool
prop_adjListForNewGraph_size (IntSmall x) = (length (adjListForNewGraph x) == x^2)

prop_adjListForNewGraph_contains :: IntSmall -> IntSmall -> Bool
prop_adjListForNewGraph_contains (IntSmall x) (IntSmall y) = (length (adjListForNewGraph x) == x^2)

prop_nodeAtVert_contains :: NodeList -> Gen Bool
prop_nodeAtVert_contains (NodeList x) = do
        let siz = length x
        i <- choose (0, (siz-1))
        if x == []
                then return True
                else return $ (nodeAtVert x i `elem` listOfNodes x )

prop_nodeList_size :: NodeList -> Bool
prop_nodeList_size (NodeList x) = (length x == length(listOfNodes x))

prop_adjListForVertex_notNull :: IntSmall -> Gen Bool
prop_adjListForVertex_notNull (IntSmall x) = do
        v <- choose (1,x)
        if x>1
                then return $ not (null (adjListForVertex x v))
                else return True


runTests = do
        --runTestTT tests
        quickCheck prop_nodeAtVert_contains
        quickCheck prop_adjListForNewGraph_contains
        quickCheck prop_adjListForNewGraph_size
        quickCheck prop_keyList_size
        quickCheck prop_adjListForVertex_notNull

