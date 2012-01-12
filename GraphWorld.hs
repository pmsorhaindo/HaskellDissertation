module GraphWorld where
import Data.Array
import Data.Graph
import qualified AntRepresent as Ant 
--sq1 :: Square
--sq1 = (mkSquare 0,0,0, (0,0))

--theWorld :: [[([Maybe Ant], Maybe Food)]]

fstTrip (x,y,z) = x 
sndTrip (x,y,z) = y
trdTrip (x,y,z) = z

squares =  array (1,100) [(i, i*i) | i <- [1..100]]


-- graphFromEdges :: Ord key => [(node,key,[key])] ->(Graph,Vertex->(node,key,[key]),key->Maybe Vertex)

siz = 6
sizSmall = 3
sizeLarge = 6
--original value x  = Small 5 - Large 8

--down
getNeighboursd x = (x`div`siz + 1 * siz) + (x`mod`siz)
--left
getNeighboursl x = x-1
--right
getNeighboursr x = x+1
--up
getNeighboursu x = ((x`div`siz -1) * siz) + (x`mod`siz)
--[left right up down]
getNeighbours x = [getNeighboursu,getNeighboursr,getNeighboursd,getNeighboursl]

--x [?(x.div.siz + 1 * siz) - (x.mod.siz),x-1,x+1, (x.div.siz - 1 * siz) - (x.mod.siz)?]
-- if edge 


--1[2,4]
--2[1,5,3]
--3[2,6]
--4[1,7,15]
--5[2,4,8,6]
--6[3,5,9]
--7[4,8]
--8[7,5,9]
--9[8,6]

--instance show Maybe Ant.Ant where 
--         show Just Ant.Ant = "Ant"
--         show Nothing = "NO ANT!"


amy2 :: Ant.Ant
amy2 = Ant.Ant (Ant.Location 0 1) (Ant.Vector Ant.North 2)

edgesToBuildEm :: [(Maybe Ant.Ant, Integer, [Integer])] -- wouldn't compile withouth this signature cause of abiguity when testing all nothing.
edgesToBuildEm = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,5]),(Nothing,5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])] -- accidently had a link to vertex 5 as Vertex 15 need to auto  generate this. 

edgesToBuild :: [(Maybe Ant.Ant, Integer, [Integer])]
edgesToBuild = [(Just amy2,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,5]),(Nothing,5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])] -- accidently had a link to vertex 5 as Vertex 15 need to auto  generate this.

edgesToBuild2 = [("rawr",1,[2,4]),("sadface",2,[1,5,3]),("waffle",3,[2,6]),("cheese",4,[1,7,15]),("maybe",5,[2,4,8,6]),("hehe",6,[3,5,9]),("cry",7,[4,8]),("lol",8,[7,5,9]),("yay",9,[8,6])]

graph' = fst $ graphFromEdges' edgesToBuild2
graphfunc' = snd $ graphFromEdges' edgesToBuild2

graph = fstTrip $ graphFromEdges edgesToBuild2
graphfunc = sndTrip $ graphFromEdges edgesToBuild2
graphfuncVert = trdTrip $ graphFromEdges edgesToBuild2

-- takes a graph for use with betterlist ofwhatisatvert
-- the second value returned from graph from Edges is the function that allows us to get values by Vertex x
betterGraphfunc x = sndTrip $ graphFromEdges x

--GraphFromEdges Result
bleep = graphFromEdges edgesToBuild2

superGraphFunc gr = sndTrip gr

--check to see if processing is needed
--processCheck = any (/=Nothing) listOfWhatIsAtVert 

--TYPE NEEEDS TO BE INT
whatIsAtVert x = fstTrip $ graphfunc x
--using map to generate a list of what the nodes contain, grab the max size of the graph array using bounds
--listOfWhatIsAtVert :: Array Vertex e -> [[Int]]
listOfWhatIsAtVert x = map whatIsAtVert [0..(snd $ bounds x)]

whatIsAtVert' gr x = fstTrip $ betterGraphfunc gr $ x
listOfWhatIsAtVert' gr x = map (whatIsAtVert' gr) [0..(snd $ bounds x)]

--splittedVertlist1 x = fst $ splitAt x listOfWhatIsAtVert
--splittedVertlist2 x = snd $ splitAt x listOfWhatIsAtVert

splittedVertlist1 x y = fst $ splitAt x y
splittedVertlist2 x y = snd $ splitAt x y

--modifiedWhatIsAtVert :: Int -> Int -> [Int] -> [Int]
modifiedWhatIsAtVert x y z = (init $ splittedVertlist1 x z) ++ [(last $ splittedVertlist1 y z)]  ++ (init $ splittedVertlist1 (y-x) (splittedVertlist2 x z)) ++ [(last $ splittedVertlist1 x z)] ++ (splittedVertlist2 (y-x) (splittedVertlist2 x z))

-- broken down for debugging
lpart1 x z = init $ splittedVertlist1 x z
lpart2 y z = last $ splittedVertlist1 y z
lpart3 x y z = init $ splittedVertlist1 (y-x) (splittedVertlist2 x z)
lpart4 x z = (last $ splittedVertlist1 x z) -- was tail ;/
lpart5 x y z = (splittedVertlist2 (y-x) (splittedVertlist2 x z))
-- yay works tested for edge cases too

brokenUpGraph z = map graphfunc (vertices z)

--verticesConntected:: Graph -> [[Int]] -- was defaulting to Integers before I put this!
verticesConntected graph = map trdTrip (brokenUpGraph graph)

updateGraph :: Int -> Int -> Array Vertex [Vertex] -> [([Char], Vertex, [Int])]
updateGraph x y graph = zip3 (modifiedWhatIsAtVert x y (listOfWhatIsAtVert graph)) (vertices graph)  (verticesConntected graph)

updateGraph' x y graph = graphFromEdges$ zip3 (modifiedWhatIsAtVert x y (listOfWhatIsAtVert graph)) (vertices graph)  (verticesConntected graph)
 

-- THE SWAP
-- listOfWhatIsAtVert graph or listOfWhatIsAtVert' edgesToBuild2 graph
-- let d = updateGraph 1 3 graph
-- let e = graphFromEdges d
-- let f = fst Trip e
-- listOfWhatIsAtVert' d f

-- legalEdges
legalEdges gr v = trdTrip $ superGraphFunc gr $ v
