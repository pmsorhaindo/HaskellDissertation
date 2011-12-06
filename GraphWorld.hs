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



--1  - 2  - 3 
-- |    |    | 
--4  - 5  - 6  
-- |    |    | 
--7  - 8 -  9 


--1[2,4]
--2[1,5,3]
--3[2,6]
--4[1,7,15]
--5[2,4,8,6]
--6[3,5,9]
--7[4,8]
--8[7,5,9]
--9[8,6]


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

squares =  array (1,100) [(i, i*i) | i <- [1..100]]

{-
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

--1[2,7]
--2[1,8,3]
--3[1,4,9]
--4[3,10,5]
--5[4,11,6]
--6[5,12]
--7[1,8,13]
--8[2,7,9,14]
--9[3,8,10,15]
--10[4,9,11,16]
--11[5,10,12,17]
--12[6,11,18]
--13[7,14,19]
--14[8,13,15,20]
--15[9,14,16,21]
--16[10,15,17,22]
--17[11,16,18,23]
--18[12,17,24]
--19[13,20,25]
--20[14,19,21,26]
--21[15,20,22,27]
--22[16,21,23,28]
--23[17,22,24,29]
--24[18,23,30]
--25[19,26,31]
--26[20,25,27,32]
--27[21,26,28,33]
--28[22,27,29,34]
--29[23,28,30,35]
--30[24,29,36]
--31[25,32]
--32[26,31,33]
--33[27,32,34]
--34[28,33,35]
--35[29,34,36]
--36[30,35] -}



