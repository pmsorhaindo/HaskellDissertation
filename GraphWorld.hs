module GridWorld where
import Data.Array
import Data.Graph
import qualified AntRepresent as Ant 
--sq1 :: Square
--sq1 = (mkSquare 0,0,0, (0,0))

--theWorld :: [[([Maybe Ant], Maybe Food)]]


{-If a programmer has a custom datatype for example an (Int,Bool,Maybe [Char]) tuple, and they want to hold all of these with the Data.Graph library, how is it possible when the the graph creation methods are defined as.

graphFromEdges :: Ord key => [(node, key, [key])] -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)Source

graphFromEdges' :: Ord key => [(node, key, [key])] -> (Graph, Vertex -> (node, key, [key]))Source

Would I be passing my custom type as a node?

Rozumiem teraz! Muszę zip my custom data type as a node with my key values! JAaaa!
-}


newEdges = [(1,2),(1,4),(2,1),(2,5),(2,3),(3,2),(3,6),(4,1),(4,7),(4,15),(5,2),(5,4),(5,8),(5,6),(6,3),(6,5),(6,9),(7,4),(7,8),(8,7),(8,5),(8,9),(9,8),(9,6)]

node1 = array (0,1) [(0,2),(1,4)]
node2 = array (0,2) [(0,1),(1,3),(2,5)]
node3 = array (0,1) [(0,2),(1,6)]
node4 = array (0,2) [(0,1),(1,7),(2,15)]
node5 = array (0,3) [(0,2),(1,4),(2,6),(3,8)]
node6 = array (0,2) [(0,3),(1,5),(2,9)]
node7 = array (0,1) [(0,4),(1,8)]
node8 = array (0,2) [(0,5),(1,7),(2,9)]
node9 = array (0,1) [(0,6),(1,8)]

--amy3 :: AntRepresent.Ant
--amy3 = AntRepresent.Ant (AntRepresent.Location 0 1) (AntRepresent.Vector North 2)

squares =  array (1,100) [(i, i*i) | i <- [1..100]]


-- graphFromEdges :: Ord key => [(node,key,[key])] ->(Graph,Vertex->(node,key,[key]),key->Maybe Vertex)

--Small
--1  - 2  - 3 
-- |    |    | 
--4  - 5  - 6  
-- |    |    | 
--7  - 8 -  9 

--Large
--1  - 2  - 3  - 4  - 5  - 6
-- |    |    |    |    |    |
--7  - 8  - 9  - 10 - 11 - 12
-- |    |    |    |    |    |
--13 - 14 - 15 - 16 - 17 - 18
-- |    |    |    |    |    |
--19 - 20 - 21 - 22 - 23 - 24
-- |    |    |    |    |    |
--25 - 26 - 27 - 28 - 29 - 30
-- |    |    |    |    |    |
--31 - 32 - 33 - 34 - 35 - 36

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
--if-} 


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

edgesToBuild = [(Just amy2,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesToBuild2 = [("rawr",1,[2,4]),("sadface",2,[1,5,3]),("waffle",3,[2,6]),("cheese",4,[1,7,15]),("maybe",5,[2,4,8,6]),("hehe",6,[3,5,9]),("cry",7,[4,8]),("lol",8,[7,5,9]),("yay",9,[8,6])]

gra = fst $ graphFromEdges' edgesToBuild

aph = snd $ graphFromEdges' edgesToBuild
