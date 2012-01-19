module World where
import Data.Array
import Data.Graph
import qualified AntRepresent as Ant

--Utils deconstructing 3 tuples with pattern matching
fstTrip (x,y,z) = x 
sndTrip (x,y,z) = y
trdTrip (x,y,z) = z


-- Adjacency List Automation
-- size = 6
-- range 1-size^2
-- left v-1 (not avaiable if v(mod)size =1)
-- right v+1 (not available if v(mod)size = 0 ((not including 0 use range)))
-- down v+size (not available if v>(size^2-size)
-- up v-size (not available if v<=size)


--test world sizes possiply should be of power two for expantion
wsize = 6
wsizSmalls = 3
wsizeLarge = 12

-- graphFromEdges :: Ord key => [(node,key,[key])] ->(Graph,Vertex->(node,key,[key]),key->Maybe Vertex)
keyList = [1..wsize*wsize]
--adjKeyList a = [genUp,genDown,genLeft,genRight] --need a list of these lists for each key in keyList
--list of Nodes -- start list of empty nodes size od wsize^2
--genUp

edgesForTestGraph :: [([Char], Int, [Int])]
edgesForTestGraph = [("rawr",1,[2,4]),("sadface",2,[1,5,3]),("waffle",3,[2,6]),("cheese",4,[1,7,15]),("maybe",5,[2,4,8,6]),("hehe",6,[3,5,9]),("cry",7,[4,8]),("lol",8,[7,5,9]),("yay",9,[8,6])]

graphTuple :: [(node, Int, [Int])] -> (Graph, Vertex -> (node, Int, [Int]), Int -> Maybe Vertex)
graphTuple edges    = graphFromEdges edges

graph edges         = fstTrip $ graphFromEdges edges
graphfunc edges     = sndTrip $ graphFromEdges edges
graphfuncVert edges = trdTrip $ graphFromEdges edges

--returns the node at vertex y from given edge structure x
nodeAtVert :: (Ord key) => [(node, key, [key])] -> Vertex -> node
nodeAtVert x y = fstTrip $ graphfunc x y

--for Edge Structure x, map the find nodeAtVert function across all the nodes in the graph.
listOfNodes x = map (nodeAtVert x) [0..(snd $ bounds (graph x))]


--list splitting functions take the first and second half, repsectively , of the list y when split at x.
splittedVertlist1 x y = fst $ splitAt x y
splittedVertlist2 x y = snd $ splitAt x y

--swapNodes :: Int -> Int -> [Int] -> [Int]
swapNodes x y z = (init $ splittedVertlist1 x z) ++ [(last $ splittedVertlist1 y z)]  ++ (init $ splittedVertlist1 (y-x) (splittedVertlist2 x z)) ++ [(last $ splittedVertlist1 x z)] ++ (splittedVertlist2 (y-x) (splittedVertlist2 x z))

-- broken down for debugging
lpart1 x z = init $ splittedVertlist1 x z
lpart2 y z = last $ splittedVertlist1 y z
lpart3 x y z = init $ splittedVertlist1 (y-x) (splittedVertlist2 x z)
lpart4 x z = (last $ splittedVertlist1 x z) -- was tail ;/ (superfail)
lpart5 x y z = (splittedVertlist2 (y-x) (splittedVertlist2 x z))
-- yay works tested for edge cases too

--brokenUpGraph :: (Graph, Vertex -> b, t) -> [b]
brokenUpGraph z = map (sndTrip z) (vertices $ fstTrip z)

--When provided a Graph combo Tuple returns the list of adjacent verts list
adjVertsFromCombo z = map trdTrip (brokenUpGraph z)

--updateGraph :: Int -> Int -> Array Vertex [Vertex] -> [([Char], Vertex, [Int])]
updateGraph x y z = zip3 (swapNodes x y (listOfNodes $ brokenUpGraph z)) (vertices $ fstTrip z)  (adjVertsFromCombo z)

--If it connects
updateGraph':: Int-> Int-> (Graph, Int -> (node, Int, [Int]), t)-> [(node, Vertex, [Int])]
updateGraph' x y z
        | y `elem` (legalEdges z $ x-1) = zip3 (swapNodes x y (listOfNodes $ brokenUpGraph z)) (vertices $ fstTrip z)  (adjVertsFromCombo z)
        | otherwise                     = []

-- legalEdges
legalEdges graphT v = trdTrip $ sndTrip graphT $ v
