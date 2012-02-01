module GraphOps where
import Data.Array
import Data.Graph
import Data.Maybe (fromJust, isNothing)
import Data.List (maximumBy)
import Test.QuickCheck
import Test.HUnit

type Size = Int
type Point = Int

-- | Utils deconstructing 3 tuples with pattern matching
fstTrip (x,y,z) = x 
sndTrip (x,y,z) = y
trdTrip (x,y,z) = z

graph edges         = fstTrip $ graphFromEdges edges
graphfunc edges     = sndTrip $ graphFromEdges edges
graphfuncVert edges = trdTrip $ graphFromEdges edges

-- | Adjacency List Automation

-- | right v-1 (not available if v(mod)size = 1) TESTED
getAdjLeft:: Size->[Int]->Point->[Int]
getAdjLeft size currentAdj pos
                                | (pos <= 0 || pos > size^2 ) || pos`mod`size == 1 = currentAdj
                                | otherwise         = (pos-1):currentAdj

-- | right v+1 (not available if v(mod)size = 0 ((not including 0 use range))) TESTED
getAdjRight:: Size->[Int]->Point->[Int]
getAdjRight size currentAdj pos
                                | (pos <= 0 || pos > size^2 ) || pos`mod`size == 0 = currentAdj
                                | otherwise                     = (pos+1):currentAdj

-- | down v+size (not available if v>(size^2-size) TESTED
getAdjDown:: Size->[Int]->Point->[Int]
getAdjDown size currentAdj pos
                                | pos > (size^2-size) || pos <= 0 = currentAdj
                                | otherwise                       = (pos+size):currentAdj

-- | up v-size (not available if v<=size) TESTED
getAdjUp:: Size->[Int]->Point->[Int]
getAdjUp size currentAdj pos
                                | (pos-size) <= 0 || pos > size^2 = currentAdj
                                | otherwise                       = (pos-size):currentAdj

adjListForVertex :: Size -> Point -> [Int]
adjListForVertex size pos = concat $ map ($pos) [(getAdjUp size []),(getAdjLeft size []),(getAdjDown size []),(getAdjRight size [])]

adjListForNewGraph :: Size -> [[Int]]
adjListForNewGraph size = map (adjListForVertex size) [1..size^2]

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

newtype Is = Is Int
        deriving (Show)

instance Arbitrary Is where
        arbitrary = do
                     --i <- (arbitrary :: Gen Int)
                     i <- choose(0,100)
                     return $ Is i

prop_keyList c1 = (length (keyList c1) == c1^2)

runTests = do
            runTestTT tests
            quickCheck prop_keyList

