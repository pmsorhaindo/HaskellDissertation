module World where
import Data.Array
import Data.Graph
import Data.Maybe (fromJust, isNothing)
import Data.List (maximumBy)
import Test.QuickCheck
import Test.HUnit
import AntRepresent
import Quadrant
import GraphOps

--Test-Framework .... to automate the testing

-- | Utils deconstructing 3 tuples with pattern matching
fstTrip (x,y,z) = x 
sndTrip (x,y,z) = y
trdTrip (x,y,z) = z

--Build a new Graph 6 and 36 need replacing with a variable graph size and graph size^2
-- let a = graphFromEdges $ zip3 [1..36] (keyList 6) (adjListForNewGraph 6)
buildEmptyWorld size = graphFromEdges $ zip3 (replicate (size^2) 0) (keyList size) (adjListForNewGraph size)

-- | Globals
a_ = graphTuple edgesForTestAGraph
b_ = graphTuple edgesForTestPGraph

--width = 3
a' = graphTuple edgesForTestAGraph -- TODO update so the graph builds with variable size
b' = graphFromEdges $ zip3 (replicate (width^2) 0) (keyList width) (adjListForNewGraph width)


