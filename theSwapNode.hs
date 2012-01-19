import GraphWorld
import Data.Array
import Data.Graph

main :: IO()
main = 
       putStr $ (show origGraph) ++ "\n" ++ (show updaListOfNodes) ++ "\n"
       where
          origGraph           = listOfWhatIsAtVert' edgesToBuild2 graph
          updaListOfNodes     = fstTrip $ graphFromEdges (updateGraph 1 3 graph)
          result              = listOfWhatIsAtVert' (updateGraph 1 3 graph) updaListOfNodes


-- The Swap
-- listOfWhatIsAtVert graph or listOfWhatIsAtVert' edgesToBuild2 graph
-- let d = updateGraph 1 3 graph
-- let e = graphFromEdges d
-- let f = fst Trip e
-- listOfWhatIsAtVert' d f
