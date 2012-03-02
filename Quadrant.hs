module Quadrant where

-- | Package imports
import Data.Array
import Data.Graph
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (maximumBy, sortBy)
import Test.QuickCheck
import Test.HUnit
--Test-Framework .... to automate the testing -- TODO

-- | Personal imports
import AntRepresent
import GraphOps

data StitchableQuads = StitchableQuads {
         qSize          :: Int
        ,relation       :: (Direction,Direction)
        ,antGraphs      :: (GraphATuple,GraphATuple)
        ,pherGraphs     :: (GraphPTuple,GraphPTuple)
        ,aEdgePair      :: ([(Maybe Ant, Int)],[(Maybe Ant, Int)])
        ,pEdgePair      :: ([(Double,Int)],[(Double,Int)]) 
        ,noProcList     :: ([Int],[Int])
        }

-- | graphFromEdges :: Ord key => [(node,key,[key])] ->(Graph,Vertex->(node,key,[key]),key->Maybe Vertex)
-- making this value easily readable
type GraphATuple = (Array Vertex [Vertex], Int -> (Maybe Ant, Point, [Point]), Point -> Maybe Vertex)
-- | similar to GraphATuple but holding pheremone levels.
type GraphPTuple = (Graph, Vertex -> (Double, Int, [Int]), Int -> Maybe Vertex)

--adjKeyList a = [genUp,genDown,genLeft,genRight] --need a list of these lists for each key in keyList
-- TODO secure these functions with better testing on numbers added AdjRight still allows ridiculos -ve numbers
-- left v-1 (not avaiable if v(mod)size =1) TESTED
-- | This function gets the adjacent node value given the Quadrant width

--Build a new Graph 6 and 36 need replacing with a variable graph size and graph size^2
-- let a = graphFromEdges $ zip3 [1..36] (keyList 6) (adjListForNewGraph 6)
buildEmptyWorld size = graphFromEdges $ zip3 (replicate (size^2) 0) (keyList size) (adjListForNewGraph size)

emptyAntQuadrant = graphFromEdges $ zip3 nodes keys adjList :: GraphATuple
        where nodes   = replicate (width^2) Nothing
              keys    = keyList width
              adjList = adjListForNewGraph width

emptyPherQuadrant = graphFromEdges $ zip3 nodes keys adjList :: GraphPTuple
        where nodes   = (replicate (width^2) 0)
              keys    = (keyList width)
              adjList = (adjListForNewGraph width)

--TEST GRAPHS
edgesForTestGraph :: [([Char], Int, [Int])]
edgesForTestGraph = [("rawr",1,[2,4]),("sadface",2,[1,5,3]),("waffle",3,[2,6]),("cheese",4,[1,7,15]),("maybe",5,[2,4,8,6]),("hehe",6,[3,5,9]),("cry",7,[4,8]),("lol",8,[7,5,9]),("yay",9,[8,6])]

edgesForTestAGraph :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Just(Ant 1 West 1),6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestPGraph :: [(Double, Int, [Int])]
edgesForTestPGraph = [(0,1,[2,4]),(0,2,[1,5,3]),(0,3,[2,6]),(0,4,[1,7,15]),(0,5,[2,4,8,6]),(0,6,[3,5,9]),(0,7,[4,8]),(0,8,[7,5,9]),(0,9,[8,6])]


graphTuple :: [(node, Int, [Int])] -> (Graph, Vertex -> (node, Int, [Int]), Int -> Maybe Vertex)
graphTuple edges    = graphFromEdges edges

graph edges         = fstTrip $ graphFromEdges edges
graphfunc edges     = sndTrip $ graphFromEdges edges
graphfuncVert edges = trdTrip $ graphFromEdges edges

--list splitting functions take the first and second half, repsectively , of the list y when split at x.
splittedVertlist1 x y = fst $ splitAt x y
splittedVertlist2 x y = snd $ splitAt x y

swapNodes :: Int -> Int -> [a] -> [a] -- huge bummer finding this bug... x < y or fail
swapNodes x y z
        | x < y = (init $ splittedVertlist1 x z) ++ [(last $ splittedVertlist1 y z)]  ++ (init $ splittedVertlist1 (y-x) (splittedVertlist2 x z)) ++ [(last $ splittedVertlist1 x z)] ++ (splittedVertlist2 (y-x) (splittedVertlist2 x z))
        | otherwise = (init $ splittedVertlist1 y z) ++ [(last $ splittedVertlist1 x z)]  ++ (init $ splittedVertlist1 (x-y) (splittedVertlist2 y z)) ++ [(last $ splittedVertlist1 y z)] ++ (splittedVertlist2 (x-y) (splittedVertlist2 y z))

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
adjVertsFromCombo :: (Graph, Vertex -> (t, t1, b), t2) -> [b]
adjVertsFromCombo z = map trdTrip (brokenUpGraph z)

updateGraph :: Int -> Int -> GraphATuple -> GraphATuple
updateGraph x y z = graphFromEdges $ zip3 (swapNodes x y (listOfNodes $ brokenUpGraph z)) ([1..])  (adjVertsFromCombo z)

--If it connects
--updateGraph':: Int-> Int-> (Graph, Int -> (node, Int, [Int]), t)-> [(node, Vertex, [Int])]
-- now changed to return Graph Tuple
updateGraph' :: Int -> Int -> GraphATuple -> Maybe GraphATuple
updateGraph' x y z
        | y `elem` (legalEdges z $ x-1) = Just (graphFromEdges $ zip3 (swapNodes x y (listOfNodes $ brokenUpGraph z)) ([1..])  (adjVertsFromCombo z))
        | otherwise                     = Nothing

-- legalEdges
legalEdges :: GraphATuple -> Int -> [Int]
legalEdges graphT v = trdTrip $ sndTrip graphT $ v


--apply something to every node in the graph At the same time possible use of `par` here?
--let epic = graphFromEdges $ zip3 (map (+1) $ map fstTrip $ brokenUpGraph a) (vertices $ fstTrip a) (adjVertsFromCombo a) -- increases by one
--forEachNode :: GraphATuple -> (t -> node) -> GraphATuple
forEachNode graphT x = graphFromEdges $ zip3 (map (x) (map (fstTrip) (brokenUpGraph graphT))) ([1..])  (adjVertsFromCombo graphT)
--BUG variable graphT left as a which was a smaller graph defined in globals... HEADACHE :/

--increase if odd
ifOdd x
        | x`mod`2 == 1 = (x+2)
        | x`mod`2 == 0 = x

--Effectively maps a function across the graph. printing Each Graph as it goes.
eachSuccNode :: GraphATuple -> Int -> IO ()
eachSuccNode graphT iterator = do
                                 putStr $ (show origGraph) ++ "\n" ++ (show $ brokenUpGraph newGraph) ++ "\n"
                                 nxt <- eachSuccNode newGraph (iterator+1)
                                 putStr "Next!"
                                 where
                                    origGraph = brokenUpGraph graphT
                                    newGraph  = updateGraph iterator (iterator+1) graphT -- swaps the nodes with the next node (currently goes out of bounds)

-- Adds an Ant to a node (pos) of a given graph (graphT)
addAnt :: GraphATuple -> Int -> GraphATuple
addAnt graphT pos = graphFromEdges $ zip3 (preList ++ [Just(Ant 1 East 1)] ++ sufList) ([1..]) (adjVertsFromCombo graphT)
        where preList | pos < 1 = []
                      | otherwise = take (pos-1) (listOfNodes $ brokenUpGraph graphT)
              sufList = drop pos (listOfNodes $ brokenUpGraph graphT)
--Allows a prexisting Ant to be placed in a graph.

addExistingAnt :: GraphATuple -> Int -> Maybe Ant -> GraphATuple
addExistingAnt graphT pos passedAnt = graphFromEdges $ zip3 (preList ++ [passedAnt] ++ sufList) ([1..]) (adjVertsFromCombo graphT)
        where preList | pos < 1 = []
                      | otherwise = take (pos-1) (listOfNodes $ brokenUpGraph graphT)
              sufList = drop pos (listOfNodes $ brokenUpGraph graphT)

--list Of nodes that need Processing
listOfNodesWithAntsIn :: Eq a => (Graph, Vertex -> (Maybe a, t1, t2), t) -> [t1]
listOfNodesWithAntsIn graphT = [vert | (ant,vert,_) <-xs , ant /= Nothing ] 
                        where xs = brokenUpGraph graphT 

--Proccess Ants at a listOfNodes - listOfNodesWithAntsIn can be used to make sure no Maybe's fall into the fromJust func.
--NEEDS TO BE A FOLD!!!
processAntsInGraph :: GraphPTuple -> GraphATuple -> [Int] -> GraphATuple
processAntsInGraph graphPT graphAT procList = foldr (procAntAtNode graphPT) graphAT procList

--processEdgeAntsGraph :: GraphPTuple -> GraphATuple -> [EdgeAntsToProcess] -> [AdditionalPherInfo] ->[nodeAvailability] -> GraphATuple
processEdgeAntsGraph graphPT graphAT procList extraPher ndAvail = foldr (procAntAtNode graphPT) graphAT procList

--Once an Ant is known to be at a Node it can be extracted with this function.
--getAntFromNode :: Num a => (t, a -> (Maybe t2, t3, t4), t1) -> a -> (Ant, Int, [Int])
getAntFromNode graphT nd = (ant,key,adjList)
                        where (Just ant,key,adjList) = sndTrip graphT $ (nd-1) -- node-1 = Vertex

getWhatIsAtNode graphT nd = fstTrip $ sndTrip graphT $ (nd-1)

isAntAtNode :: GraphATuple -> Int -> Bool
isAntAtNode graphT nd =  not $ isNothing presence 
                        where  (presence,_,_) = sndTrip graphT $ (nd-1)

-- Calculates the Maybe Target Node
calcTargetNode :: Size -> (Ant, Point, t) -> [Int]
calcTargetNode siz antNode 
               | direction == North = getAdjUp siz [] pos
               | direction == South = getAdjDown siz [] pos
               | direction == West  = getAdjLeft siz [] pos
               | direction == East  = getAdjRight siz [] pos
                        where direction = antDir $ fstTrip antNode
                              pos       = sndTrip antNode

--Move an Ant
--TODO
--note keys are 1 based Vertex's are 0 based... the death of me 'twill be!
-- Get Ant from Vert %% (ant,node,adjList) = sndTrip graphT $ (vert-1) -DONE
-- Determine Ants dir n' targetVert %%  - DONE!
-- Calculate what is at target Vert (Realized this could be an isAntAtNode func) - Done
------- If Nothing (Now If False) SwapNode;recalculate dir - Doneish (Stays still if no node to move to
------- If Ant (Now If True) chill in Square (RECALCULATE dir)

-- ONLY PUT ANTS INTO THIS FUNCTION
-- If provided with an AntNode will move the antNode if Ant can move.

moveAnt :: GraphATuple -> Int -> GraphATuple
moveAnt graphT nd
        | targetV == [] = graphT
        | not (isAntAtNode graphT (head targetV)) = updateGraph nd (head targetV) graphT --isAntAtNode graphT (head targetV)
        | otherwise = graphT
                where targetV = (calcTargetNode (truncate (sqrt(fromIntegral maxNodeOfGraph))) (getAntFromNode graphT nd))
                         where maxNodeOfGraph = (snd $ bounds $ fstTrip graphT)+1

-- Process an Ant
-- Sense Surroundings [NESW] DONE
-- Decide on Best Action (factoring in last action?) Set Dir DONE
-- Move - Done!

procAntAtNode :: GraphPTuple -> Int -> GraphATuple -> GraphATuple
procAntAtNode graphPT nd graphAT = do 
                           let pherLevels = senseSur graphPT nd -- still needed for recalculation of Dir.. ect
                           let newDir = makeDecision pherLevels -- DONE
                           let graphAT' = setDir graphAT nd newDir
                           let graphT2 = moveAnt graphAT' nd
                           graphT2

senseSur :: GraphPTuple -> Int -> [(Direction,Double)]
senseSur graphT nd = map directionize (adjListForVertex (truncate (sqrt(fromIntegral(snd $ bounds $ fstTrip graphT)+1))) nd)
                where directionize x
                                | x == nd+1 = (East, (fstTrip $ (sndTrip graphT) (x-1))) -- Sort out function composition to make neater.
                                | x == nd-1 = (West, (fstTrip $ (sndTrip graphT) (x-1)))
                                | x > nd = (South, (fstTrip $ (sndTrip graphT) (x-1)))
                                | x < nd = (North, (fstTrip $ (sndTrip graphT) (x-1)))

-- | increaseSense --TODO
increaseSense :: [(Direction,Double)] -> [(Direction,Double)]
increaseSense = undefined
        
--procEdgeAntAtNode :: StitchableQuads -> Int -> StitchableQuads 
procEdgeAntAtNode qs pos  = do -- rename fst and snd to adj and curr to make more readable?

        let rel = relation qs 
        let currAntGraph = fst $ antGraphs qs
        let currPherGraph = fst $ pherGraphs qs
        let adjAntGraph = snd $ antGraphs qs
        let adjPherGraph = snd $ pherGraphs qs
        let aEdge = aEdgePair qs
        let pEdge = pEdgePair qs
        let npList = noProcList qs
        let resultingTuples = undefined

        let currAnt = (fst $ (fst $ aEdge) !! pos)
        let adjAnt = (fst $ (snd $ aEdge) !! pos)
        
        let x | (isNothing currAnt) && (isNothing adjAnt)= procEdgeAntAtNode qs (pos+1)
              | (isJust currAnt) && (isNothing adjAnt) = loneEdgeAnt qs True pos
              | (isNothing currAnt) && (isJust adjAnt) = loneEdgeAnt qs False pos
              | (isJust currAnt) && (isJust adjAnt) = doubleEdgeAnt qs (pos+1)

        if pos<10
                then x
                else qs

data WhichSide = Side | OtherSide

getSide :: Bool -> WhichSide -> (a,a) -> a
getSide True Side = fst
getSide False Side = snd
getSide True OtherSide = snd
getSide False OtherSide = fst
        

-- | Lone Edge Ant
loneEdgeAnt :: StitchableQuads -> Bool -> Int -> StitchableQuads
loneEdgeAnt qs isCurr pos = do
                        let side = getSide isCurr Side
                        let oSide = getSide isCurr OtherSide
                        
                        --Move Ant -- tested with hand calculations
                        let a = fromJust $ fst((side $ aEdgePair $ qs)!!pos) -- gets me my Ant
                        let nd = nodeFromEdgeIndex side qs pos
                        -- fetch sense from the graph the ant is in.
                        let snse = senseSur (side $ pherGraphs qs) nd
                        --increases sense to make use of the other graph.
                        let isnse = ((side$relation qs), (fst(oSide (pEdgePair qs)!!pos))) :snse
                        -- makes decision by prioritizing the phermone list
                        let decisions = makeDecisions isnse

                        let moveOutDir = side $ relation qs -- if this direction is chosen, special swap needed
                        let moveBackDir = oppDir (antDir a) -- if this direction is chosen change direction but don't move.

                        let qs = loneMoveIt side qs moveOutDir moveBackDir nd decisions isCurr --swapNode --addToNoProc

                        procEdgeAntAtNode qs (pos+1)

loneMoveIt  :: (((GraphATuple, GraphATuple), (GraphATuple, GraphATuple)) -> (GraphATuple, GraphATuple)) -> StitchableQuads -> Direction -> Direction -> Int -> [(Direction, b)] -> Bool -> StitchableQuads
loneMoveIt side qs mod mbd nd (dec:decs) isCurr = loneMoveIt' side qs mod mbd nd (dec:decs) isCurr
loneMoveIt _ qs _ _ _ [] _ = qs

loneMoveIt' side qs mod mbd nd (dec:decs) isCurr | mod == fst dec = checkForAntOut side qs mod mbd nd (dec:decs) isCurr
                                                 | mbd == fst dec = flipAntAtNode side qs mbd nd
                                                 | otherwise = checkForAntIn side qs mod mbd nd (dec:decs) isCurr

flipAntAtNode side qs mbd nd = newQs qs
        where newQs qs = StitchableQuads (quadSize qs) (rel qs) (ags qs side) (pgs qs) (aep qs) (pep qs) (npl qs)
              quadSize qs = (qSize qs)
              rel qs = (relation qs)
              ags qs side = side (((setDir (fst$antGraphs qs) nd mbd),(snd$antGraphs qs)),
                                 ((fst$antGraphs qs),(setDir (snd$antGraphs qs) nd mbd))) -- depending on which side the Ant is in
                                                                                          -- a particular solution is chosen
              pgs qs = (pherGraphs qs)
              aep qs = (aEdgePair qs)
              pep qs = (pEdgePair qs)
              npl qs = (noProcList qs) -- NEEDS CHANGING
-- (((setDir (fst$antGraphs qs) nd mbd),(snd$antGraphs qs)),(fst$antGraphs qs),((setDir (snd$antGraphs qs) nd mbd)))
-- (((GraphATuple, GraphATuple),GraphATuple,GraphATuple) -> t0) -> t0

checkForAntIn side qs mod mbd nd (dec:decs) isCurr = do
                        let nxtNd = nextNode nd (fst dec) $ qSize qs
                        if isAntAtNode (fst$antGraphs qs) nxtNd
                                then loneMoveIt side qs mod mbd nd decs isCurr
                                else swapIn qs side nd nxtNd

--swapIn :: StitchableQuads -> (((GraphATuple, GraphATuple), (GraphATuple, GraphATuple)) -> (GraphATuple, GraphATuple)) -> Int -> Int -> StitchableQuads
swapIn qs side nd1 nd2 = newQs qs
                where newQs qs = StitchableQuads (quadSize qs) (rel qs) (ags qs side) (pgs qs) (aep qs) (pep qs) (npl qs)
                      quadSize qs = (qSize qs)
                      rel qs = (relation qs)
                      ags qs side = side (((updateGraph nd1 nd2 (fst$antGraphs qs)),(snd$antGraphs qs)),
                                         ((fst$antGraphs qs),((updateGraph nd1 nd2 (snd$antGraphs qs)))))--depending on which side the Ant is in
                                                                                                        -- a particular solution is chosen
                      pgs qs = (pherGraphs qs)
                      aep qs = (aEdgePair qs)
                      pep qs = (pEdgePair qs)
                      npl qs = (noProcList qs) --TODO                   
                        --updateGraph nd1 nd2 side$$antGraphs -> GraphATuple
                        --(((updateGraph nd1 nd2 (fst$antGraphs)),(snd$antGraphs qs)),
                        --                 ((fst$antGraphs qs),(updateGraph nd1 nd2 (fst$antGraphs))))

--swapOut :: StitchableQuads -> (((GraphATuple, GraphATuple), (GraphATuple, GraphATuple)) -> (GraphATuple, GraphATuple)) -> Int -> Int
--        -> StitchableQuads
swapOut qs side side' currNd oppNd = newQs qs
                where newQs qs = StitchableQuads (quadSize qs) (rel qs) (ags qs side) (pgs qs) (aep qs) (pep qs) (npl qs)
                      quadSize qs = (qSize qs)
                      rel qs = (relation qs)
                      tempAnt = Just (fstTrip (getAntFromNode (side' (antGraphs qs)) currNd)) --actually doesn't because its only one Ant
                      ags qs side = side(((addExistingAnt (fst$antGraphs qs) currNd Nothing),(addExistingAnt (snd$antGraphs qs) oppNd tempAnt)),(((addExistingAnt (fst$antGraphs qs) currNd tempAnt)),((addExistingAnt (snd$antGraphs qs) currNd Nothing))))

--ags qs side = side (((addExistingAnt (fst$antGraphs qs) currNd Nothing),(addExistingAnt (snd$antGraphs qs) oppNd tempAnt)), (((addExistingAnt (fst$antGraphs qs) currNd tempAnt)),((addExistingAnt (snd$antGraphs qs) currNd Nothing))))--depending on which side the Ant is in
                                                                                                        -- a particular solution is chosen
                      pgs qs = (pherGraphs qs)
                      aep qs = (aEdgePair qs)
                      pep qs = (pEdgePair qs)
                      npl qs = (noProcList qs)

checkForAntOut side qs mod mbd nd (dec:decs) isCurr = do 
                let outNd = outNode nd (fst dec) $ qSize qs
                let side' = getSide isCurr Side
                if isAntAtNode (fst$antGraphs qs) outNd
                        then loneMoveIt side qs mod mbd nd decs isCurr
                        else swapOut qs side side' nd outNd -- side' to selct the section of the antGraph returned from antGraphs I want.



-- Y U NO WORK FOR ME :( - found that work around tho :D
otherSide side | side == fst = snd
               | side == snd = fst

-- | Getting the node value of the node in a given direction on the same graph
nextNode :: Int -> Direction -> Int -> Int
nextNode nd dir siz | dir == North = nd - siz
                    | dir == South = nd + siz
                    | dir == East = nd + 1
                    | dir == West = nd - 1

-- | Getting the node value of the node in a given direction on another graph given an edge Node.
outNode :: Int -> Direction -> Int -> Int
outNode nd dir siz | dir == North = nd + (siz^2 - siz)
                   | dir == South = nd - (siz^2 - siz)
                   | dir == East = nd - (siz-1)
                   | dir == West = nd + (siz-1)

nodeFromEdgeIndex :: ((Direction, Direction) -> Direction) -> StitchableQuads -> Int -> Int
nodeFromEdgeIndex side qs pos | (side $ relation qs) == North = (pos+1) -- edgepair index to node.
                              | (side $ relation qs) == South = (pos+1) +((qSize qs)^2 - (qSize qs))
                              | (side $ relation qs) == East = (qSize qs)*(pos)+1
                              | (side $ relation qs) == West = (pos+1)*(qSize qs)

-- | Double Edge Ant
doubleEdgeAnt :: StitchableQuads -> Int -> StitchableQuads
doubleEdgeAnt qs pos = do
                --TODO STUFF
                --Move Ant
                procEdgeAntAtNode qs (pos+1)


-- | Arranges the Pheremone list to hold
makeDecision :: [(Direction,Double)] -> Direction
makeDecision pLevels = fst (maximumBy highestPher pLevels)
                where highestPher x y = (snd x) `compare` (snd y) 

makeDecisions :: [(Direction,Double)] -> [(Direction,Double)]
makeDecisions pLevels = sortBy highestPher pLevels
                where highestPher x y = (snd x) `compare` (snd y) 

-- | Change an Ants current direction at a given node to a given direction.
setDir :: GraphATuple -> Point -> Direction -> GraphATuple
setDir graphT nd newDir = addExistingAnt graphT nd (modif $ fstTrip $ getAntFromNode graphT nd) -- Not to be called it there isn't an ant at the node getAntFromNode will have a fit.
                where modif x = Just (Ant (antId x) newDir (pherLevel x)) 

-- | Function to process the whole Quadrant.
processAQuadrant :: GraphATuple -> GraphPTuple -> GraphATuple 
processAQuadrant graphAT graphPT = do    
                                   let nodesToProcess = listOfNodesWithAntsIn graphAT
                                   let graphAT' = processAntsInGraph graphPT graphAT nodesToProcess
                                   graphAT'

processAQuadrant_ :: (GraphATuple, GraphPTuple) -> GraphATuple 
processAQuadrant_ graphs = do    
                           let graphAT = fst graphs
                           let graphPT = snd graphs
                           let nodesToProcess = listOfNodesWithAntsIn graphAT
                           let graphAT' = processAntsInGraph graphPT graphAT nodesToProcess
                           graphAT'

-- | Export graph Edge for stitching
--getEdge :: GraphATuple -> Direction -> Size -> [a] --TODO package the dir up with it.. ([a],Direction)
getAEdge graphAT getDir
        | getDir == North = zip (map f [1 .. size]) [1..size]
        | getDir == West  = zip (map f [size,size+size .. size^2]) [size,size+size .. size^2]
        | getDir == East  = zip (map f [1,size+1 .. (size^2-(size-1))]) [1,size+1 .. (size^2-(size-1))]
        | getDir == South = zip (map f [(size^2-(size-1)) .. (size^2)]) [(size^2-(size-1)) .. (size^2)]
               where size = truncate (sqrt(fromIntegral ((snd $ bounds $ fstTrip graphAT) + 1))) :: Int
                     f    = getWhatIsAtNode graphAT

--getPEdge :: GraphPTuple -> Direction -> Size -> [a] --TODO package the dir up with it.. ([a],Direction)
getPEdge graphPT getDir
        | getDir == North = zip (map f [1 .. size]) [1..size]
        | getDir == West  = zip (map f [size,size+size .. size^2]) [size,size+size .. size^2]
        | getDir == East  = zip (map f [1,size+1 .. (size^2-(size-1))]) [1,size+1 .. (size^2-(size-1))]
        | getDir == South = zip (map f [(size^2-(size-1)) .. (size^2)]) [(size^2-(size-1)) .. (size^2)]
               where size = truncate (sqrt(fromIntegral ((snd $ bounds $ fstTrip graphPT) + 1))) :: Int
                     f    = getWhatIsAtNode graphPT

-- | TODO
processPhers :: GraphPTuple -> GraphPTuple
processPhers = undefined --TODO
-- | TODO
transPherToAnt :: GraphATuple -> GraphPTuple -> GraphATuple
transPherToAnt graphAT graphPTuple = undefined --do
                                      --let nodesToProcess = listOfNodesWithAntsIn graphAT
                                      --TODO
-- | TODO
transAntToPher :: GraphPTuple -> GraphATuple -> GraphPTuple
transAntToPher = undefined -- TODO

-- | Globals
a'' = graphTuple edgesForTestAGraph
b'' = graphTuple edgesForTestPGraph

width = 3
a = graphFromEdges $ zip3 (replicate (width^2) Nothing) (keyList width) (adjListForNewGraph width) :: GraphATuple
b = graphFromEdges $ zip3 (replicate (width^2) 0) (keyList width) (adjListForNewGraph width) :: GraphPTuple


