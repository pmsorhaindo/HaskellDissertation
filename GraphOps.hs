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


-- | Adjacency List Automation

getAdjLeft:: Size->[Int]->Point->[Int]
getAdjLeft size currentAdj pos
                                | (pos <= 0 || pos > size^2 ) || pos`mod`size == 1 = currentAdj
                                | otherwise         = (pos-1):currentAdj

-- right v+1 (not available if v(mod)size = 0 ((not including 0 use range))) TESTED
getAdjRight:: Size->[Int]->Point->[Int]
getAdjRight size currentAdj pos
                                | (pos <= 0 || pos > size^2 ) || pos`mod`size == 0 = currentAdj
                                | otherwise                     = (pos+1):currentAdj

-- down v+size (not available if v>(size^2-size) TESTED
getAdjDown:: Size->[Int]->Point->[Int]
getAdjDown size currentAdj pos
                                | pos > (size^2-size) || pos <= 0 = currentAdj
                                | otherwise                       = (pos+size):currentAdj

-- up v-size (not available if v<=size) TESTED
getAdjUp:: Size->[Int]->Point->[Int]
getAdjUp size currentAdj pos
                                | (pos-size) <= 0 || pos > size^2 = currentAdj
                                | otherwise                       = (pos-size):currentAdj

--This function nearly took forever but thanks to LYAH's awesome explanation on $ (function application I bust it out in a train journey :D
--adjListForVertex :: Point->Size->[Int]
adjListForVertex size pos = concat $ map ($pos) [(getAdjUp size []),(getAdjLeft size []),(getAdjDown size []),(getAdjRight size [])]

adjListForNewGraph size = map (adjListForVertex size) [1..size^2]

--list of Nodes -- start list of empty nodes size od size^2
keyList :: Int -> [Int]
keyList size = [1..size^2]


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

