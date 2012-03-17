{-# OPTIONS -fwarn-name-shadowing -XRankNTypes -XFlexibleContexts #-} 
module Quadrant where

-- | Package imports
import Data.Array
import Data.Graph
import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (maximumBy, sortBy)
import Data.Bool.HT
import Debug.Trace
import Test.QuickCheck
import Test.HUnit
--Test-Framework .... to automate the testing -- TODO

-- | Personal imports
import AntRepresent
import GraphOps

type GraphSelect = ((GraphATuple, GraphATuple), (GraphATuple, GraphATuple)) -> (GraphATuple, GraphATuple)

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
-- left v-1 (not avaiable if v(modir) size =1) TESTED
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
edgesForTestAGraph = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Just(Ant 1 West 1.0 0 Return),6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Just(Ant 1 West 0.2 0 Return),9,[8,6])]

edgesForTestAGraph1' :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph1' = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestAGraph2' :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph2' = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Just(Ant 1 West 2.1  0 Return),5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestAGraph3' :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph3' = [(Just(Ant 1 West 2.1  0 Return),1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestAGraph4' :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph4' = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Just(Ant 1 West 2.1  0 Return),6,[3,5,9]),(Just(Ant 1 West 2.1  0 Return),7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestPGraph :: [(Double, Int, [Int])]
edgesForTestPGraph = [(0,1,[2,4]),(0,2,[1,5,3]),(0,3,[2,6]),(0,4,[1,7,15]),(0,5,[2,4,8,6]),(0,6,[3,5,9]),(0,7,[4,8]),(0,8,[7,5,9]),(0,9,[8,6])]


graphTuple :: [(node, Int, [Int])] -> (Graph, Vertex -> (node, Int, [Int]), Int -> Maybe Vertex)
graphTuple edgs    = graphFromEdges edgs

graph edgs         = fstTrip $ graphFromEdges edgs
graphfunc edgs     = sndTrip $ graphFromEdges edgs
graphfuncVert edgs = trdTrip $ graphFromEdges edgs

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
addAnt graphT pos = graphFromEdges $ zip3 (preList ++ [Just(Ant 1 East 1 0 Return)] ++ sufList) ([1..]) (adjVertsFromCombo graphT)
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
                                | otherwise = undefined

-- | increaseSense --TODO
increaseSense :: [(Direction,Double)] -> [(Direction,Double)]
increaseSense = undefined

putItOutside :: StitchableQuads -> Int -> Maybe Ant -> Maybe Ant -> StitchableQuads
putItOutside qs pos currAnt adjAnt
                      | (isNothing currAnt) && (isNothing adjAnt) = procEdgeAntAtNode qs (1+pos)
                      | (isJust currAnt) && (isNothing adjAnt)    = loneEdgeAnt qs True pos
                      | (isNothing currAnt) && (isJust adjAnt)    = loneEdgeAnt qs False pos
                      | (isJust currAnt) && (isJust adjAnt)       = loneEdgeAnt qs False pos
putItOutside _ _ _ _ = undefined
        

-- |
procEdgeAntAtNode :: StitchableQuads -> Int -> StitchableQuads
procEdgeAntAtNode qs pos  = do -- rename fst and snd to adj and curr to make more readable?
        
        let rel = relation qs 
        let currAntGraph = fst $ antGraphs qs
        let currPherGraph = fst $ pherGraphs qs
        let adjAntGraph = snd $ antGraphs qs
        let adjPherGraph = snd $ pherGraphs qs
        let aEdge = aEdgePair qs
        let pEdge = pEdgePair qs
        let npList = noProcList qs

        let currAnt = (fst $ (fst $ aEdge) !! pos)
        let adjAnt = (fst $ (snd $ aEdge) !! pos)
        
        if pos<(qSize qs) -- fail
                then putItOutside qs pos currAnt adjAnt
                else qs

-- TODO THIS IS FOR TESTING PUROSES!!
--testProc :: forall a a1. (Ord a1, Ord a, Num a, Num a1) => a -> IO ()
testProc pos = do
        if pos <3
                then testfunc pos 3 3
                else putStrLn("Termin")

--testfunc :: forall a a1. (Ord a1, Ord a, Num a, Num a1) => a -> a1 -> a1 -> IO ()
testfunc pos jub bub
        | jub==bub = testProc (pos+1)
        | jub > bub = yolo pos

--yolo :: forall a a1. (Ord a1, Ord a, Num a, Num a1) => a -> IO ()
yolo pos = do
        putStrLn("yo!")
        testProc (pos+1)

data WhichSide = Side | OtherSide

getSide :: Bool -> WhichSide -> (a,a) -> a
getSide True Side = fst
getSide False Side = snd
getSide True OtherSide = snd
getSide False OtherSide = fst
        

-- | Lone Edge Ant
loneEdgeAnt :: StitchableQuads -> Bool -> Int -> StitchableQuads
loneEdgeAnt qs isCurr pos = do
        let side        = getSide isCurr Side
        let oSide       = getSide isCurr OtherSide

        --Move Ant -- tested with hand calculations
        let a           = fromJust $ fst((side $ aEdgePair $ qs)!!pos) -- gets me my Ant
        let nd          = nodeFromEdgeIndex side qs pos
        -- fetch sense from the graph the ant is in.
        let snse        = senseSur (side $ pherGraphs qs) nd
        --increases sense to make use of the other graph.
        let isnse       = ((side$relation qs), (fst(oSide (pEdgePair qs)!!pos))) :snse
        -- makes decision by prioritizing the phermone list
        let decisions   = makeDecisions isnse

        let moveOutDir  = side $ relation qs -- if this direction is chosen, special swap needed
        let moveBackDir = oppDir (antDir a) -- if this direction is chosen change direction but don't move.

        let newQs = loneMoveIt side qs moveOutDir moveBackDir nd decisions isCurr --swapNode --addToNoProc

        --procEdgeAntAtNode qs (pos+1)
        newQs

-- | 
loneMoveIt  :: GraphSelect -> StitchableQuads -> Direction -> Direction -> Int -> [(Direction, Double)] -> Bool -> StitchableQuads
loneMoveIt side qs modir mbdir nd (dec:decs) isCurr = loneMoveIt' side qs modir mbdir nd (dec:decs) isCurr
loneMoveIt _ qs _ _ _ [] _ = qs -- No possible moves.


loneMoveIt' :: GraphSelect -> StitchableQuads -> Direction -> Direction -> Int -> [(Direction, Double)] -> Bool -> StitchableQuads
loneMoveIt' side qs modir mbdir nd (dec:decs) isCurr | modir == fst dec = checkForAntOut side qs modir mbdir nd (dec:decs) isCurr
                                                     | mbdir == fst dec = flipAntAtNode side qs mbdir nd
                                                     | otherwise = checkForAntIn side qs modir mbdir nd (dec:decs) isCurr
loneMoveIt' _ _ _ _ _ [] _ = undefined



-- | Changes the direction an ant so they are facing in the opposite direction.
flipAntAtNode :: GraphSelect -> StitchableQuads -> Direction -> Point-> StitchableQuads
flipAntAtNode side qs mbdir nd = newQs qs
        where newQs x    = StitchableQuads (quadSize x) (rel x) (ags x side) (pgs x) (aep x) (pep x) (npl x)
              quadSize x = (qSize qs)
              rel x      = (relation qs)
              ags x gs   = gs (((setDir (fst$antGraphs qs) nd mbdir),(snd$antGraphs qs)),
                                 ((fst$antGraphs qs),(setDir (snd$antGraphs qs) nd mbdir)))
              -- depending on which side the Ant is in a particular solution is chosen
              pgs x = (pherGraphs qs)
              aep x = (aEdgePair qs)
              pep x = (pEdgePair qs)
              npl x = (noProcList qs) -- NEEDS CHANGING

-- |
checkForAntIn :: (((GraphATuple, GraphATuple), (GraphATuple, GraphATuple)) -> (GraphATuple, GraphATuple)) -> StitchableQuads -> Direction -> Direction -> Int -> [(Direction, Double)] -> Bool -> StitchableQuads
checkForAntIn side qs modir mbdir nd (dec:decs) isCurr = do
        let nxtNd = nextNode nd (fst dec) $ qSize qs
        if isAntAtNode (fst$antGraphs qs) nxtNd
                then loneMoveIt side qs modir mbdir nd decs isCurr
                else swapIn qs side nd nxtNd

-- |
swapIn :: StitchableQuads -> GraphSelect -> Int -> Int -> StitchableQuads
swapIn qs side nd1 nd2   = newQs qs
        where newQs x    = StitchableQuads (quadSize x) (rel x) (ags x side) (pgs x) (aep x) (pep x) (npl x)
              quadSize x = (qSize qs)
              rel x      = (relation qs)
              ags x gs   = gs (((updateGraph nd1 nd2 (fst$antGraphs qs)),(snd$antGraphs qs)),
                                 ((fst$antGraphs qs),((updateGraph nd1 nd2 (snd$antGraphs qs)))))
                --depending on which side the Ant is in a particular solution is chosen
              pgs x = (pherGraphs qs)
              aep x = (aEdgePair qs)
              pep x = (pEdgePair qs)
              npl x = (noProcList qs) --TODO Update no proc List      

-- |
checkForAntOut :: GraphSelect -> StitchableQuads -> Direction -> Direction -> Int -> [(Direction, Double)] -> Bool -> StitchableQuads
checkForAntOut side qs modir mbdir nd (dec:decs) isCurr = do 
        let outNd = outNode nd (fst dec) $ qSize qs
        let side' = getSide isCurr Side
        if isAntAtNode (fst$antGraphs qs) outNd
                then loneMoveIt side qs modir mbdir nd decs isCurr
                else swapOut qs side side' nd outNd -- side' to selct the section of the antGraph returned from antGraphs I want.
checkForAntOut  _ _ _ _ _ [] _ = undefined

-- | 
swapOut :: forall t1 t2 t t3. StitchableQuads -> GraphSelect -> ((GraphATuple, GraphATuple) -> (t, Int -> (Maybe Ant, t1, t2), t3)) -> Int -> Int -> StitchableQuads
swapOut qs side side' currNd oppNd = newQs qs
        where newQs x    = StitchableQuads (quadSize x) (rel x) (ags x side) (pgs x) (aep x) (pep x) (npl x)
              quadSize x = qSize qs
              rel x      = relation qs
              tempAnt     = Just (fstTrip (getAntFromNode (side' (antGraphs qs)) currNd)) 
              -- actually doesn't because its only one Ant depending on which side the Ant is in
              -- a particular solution is chosen from the tuple.                    
              ags x gs   = gs(((addExistingAnt (fst$antGraphs qs) currNd Nothing),(addExistingAnt (snd$antGraphs qs) oppNd tempAnt)),(((addExistingAnt (fst$antGraphs qs) currNd tempAnt)),((addExistingAnt (snd$antGraphs qs) currNd Nothing))))
              pgs x      = pherGraphs qs
              aep x      = aEdgePair qs
              pep x      = pEdgePair qs
              npl x      = noProcList qs

{-      *Main> let sti = stitchUpEdge antWorld pherWorld ((0,East),(1,West)) 3
        *Main> let x = swapOut sti fst snd 6 4 
        *Main> let gra = antGraphs  x
        *Main> prettyAnt $ fst $antGraphs sti 
        |X||X||X|
        |X||X||O|
        |X||X||O|
        .        
        
        *Main> prettyAnt $fst gra
        |X||X||X|
        |X||X||X|
        |X||X||O|
        .
        
        *Main> prettyAnt $snd gra
        |X||X||X|
        |O||X||O|
        |X||X||O|
        . -}


-- Y U NO WORK FOR ME :( - found that work around tho :D
otherSide :: forall a b. Eq ((a, a) -> a) => ((a, a) -> a) -> (b, b) -> b
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

nodeFromEdgeIndex :: ((Direction, Direction) -> Direction) -> StitchableQuads -> Int -> Int -- TODO Faulty function detected
nodeFromEdgeIndex side qs pos | (side $ relation qs) == North = (pos+1) -- edgepair index to node.
                              | (side $ relation qs) == South = (pos+1) +((qSize qs)^2 - (qSize qs))
                              | (side $ relation qs) == East = (qSize qs)*(pos+1)
                              | (side $ relation qs) == West = (pos+1)*(qSize qs)

-- | Double Edge Ant
doubleEdgeAnt :: StitchableQuads -> Int -> StitchableQuads
doubleEdgeAnt qs pos = do
        let a1          = fromJust $ fst((fst $ aEdgePair $ qs)!!pos)
        let a2          = fromJust $ fst((snd $ aEdgePair $ qs)!!pos)
        let nd1         = nodeFromEdgeIndex fst qs pos
        let nd2         = nodeFromEdgeIndex snd qs pos

        let snse1       = senseSur (fst $ pherGraphs qs) nd1
        let snse2       =  senseSur (snd $ pherGraphs qs) nd2
        let isnse1      = ((fst$relation qs), (fst(snd (pEdgePair qs)!!pos))) :snse1
        let isnse2      = ((snd$relation qs), (fst(fst (pEdgePair qs)!!pos))) :snse2

        let decs1  = makeDecisions isnse1
        let decs2  = makeDecisions isnse2

        let moveOutDir1 = fst $ relation qs
        let moveInDir1  = oppDir (antDir a1)
        let moveOutDir2 = snd $ relation qs
        let moveInDir2  = oppDir (antDir a2)    

        let newQs =  doubleMoveIt qs (moveOutDir1,moveOutDir2) (moveInDir1,moveInDir2) (nd1,nd2) (decs1,decs2) (False,False) :: StitchableQuads

        procEdgeAntAtNode newQs (pos+1)


-- | 
doubleMoveIt   :: StitchableQuads -> (Direction, Direction) -> (Direction, Direction) -> (Int,Int) -> ([(Direction, Double)], [(Direction, Double)])-> (Bool, Bool)-> StitchableQuads
doubleMoveIt qs modir mbdir nd (dec1:decs1,dec2:decs2) (False,False) = do
        let attemptMove = select  (doubleMoveIt qs modir mbdir nd (dec1:decs1,decs2) (False,False))  $
                --"attempt to Move Ant1 back in"
                (fst dec1 == oppDir(fst modir)  ,loneMoveIt (getSide True Side) qs (fst modir)  (fst mbdir) (fst nd) (dec1:decs1) True  ): 
                --"attempt to Move Ant2 back in"
                (fst dec2 == oppDir(snd modir)  ,loneMoveIt (getSide False Side) qs (snd modir)  (snd mbdir) (snd nd) (dec2:decs2) False):
                --"attempt to Move Ant1 along Edge"
                (not (fst dec1 ==  (fst modir) ),loneMoveIt (getSide True Side) qs (fst modir)  (snd mbdir) (fst nd) (dec1:decs1) True  ):
                --"attempt to Move Ant2 along Edge"
                (not (fst dec2 ==  (snd modir) ),loneMoveIt (getSide False Side) qs (fst modir)  (snd mbdir) (snd nd) (dec2:decs2) False): 
                -- needs to be random which one to remove
                (True                        ,doubleMoveIt qs modir mbdir nd (dec1:decs1,decs2) (False,False)): 
                []
        attemptMove

doubleMoveIt _ _ _ _ ([], _) _ = undefined
doubleMoveIt _ _ _ _ (_ : _, []) _ = undefined
doubleMoveIt _ _ _ _ (_ : _, _ : _) (True, _) = undefined
doubleMoveIt _ _ _ _ (_ : _, _ : _) (False, True) = undefined




doubleMoveOne :: GraphSelect -> StitchableQuads -> Direction -> Direction -> Int -> [(Direction, Double)] -> Bool -> StitchableQuads
doubleMoveOne side qs modir mbdir nd (dec:decs) isCurr = doubleMoveOne' side qs modir mbdir nd (dec:decs) isCurr
doubleMoveOne _ qs _ _ _ [] _ = qs -- No possible moves.

doubleMoveOne' :: GraphSelect -> StitchableQuads -> Direction -> Direction -> Int -> [(Direction, Double)] -> Bool -> StitchableQuads
doubleMoveOne' side qs modir mbdir nd (dec:decs) isCurr | modir == fst dec = checkForAntOut side qs modir mbdir nd (dec:decs) isCurr
                                                    | mbdir == fst dec = flipAntAtNode side qs mbdir nd
                                                    | otherwise = checkForAntIn side qs modir mbdir nd (dec:decs) isCurr
doubleMoveOne' _ _ _ _ _ [] _ = undefined

-- | Arranges the Pheremone list to hold
makeDecision :: [(Direction,Double)] -> Direction
makeDecision pLevels = fst (maximumBy highestPher pLevels)
                where highestPher x y = (snd x) `compare` (snd y) 

-- |
makeDecisions :: [(Direction,Double)] -> [(Direction,Double)]
makeDecisions pLevels = sortBy highestPher pLevels
                where highestPher x y = (snd x) `compare` (snd y) 

-- | Change an Ants current direction at a given node to a given direction.
setDir :: GraphATuple -> Point -> Direction -> GraphATuple
setDir graphT nd newDir = addExistingAnt graphT nd (modif $ fstTrip $ getAntFromNode graphT nd) -- Not to be called it there isn't an ant at the node getAntFromNode will have a fit.
                where modif x = Just (Ant (antId x) newDir (pherLevel x) (age x) (aim x)) 

-- | Function to process the whole Quadrant.
processAQuadrant :: GraphATuple -> GraphPTuple -> GraphATuple 
processAQuadrant graphAT graphPT = do    
        let nodesToProcess = listOfNodesWithAntsIn graphAT
        let graphAT' = processAntsInGraph graphPT graphAT nodesToProcess
        graphAT'

-- |
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

getAEdge _ _ = undefined

-- |
getPEdge :: forall t1 t2 a e t3 a1. (Ix a, Integral a) => (Array a e, Int -> (a1, t1, t2), t3) -> Direction -> [(a1, Int)]
getPEdge graphPT getDir
        | getDir == North = zip (map f [1 .. size]) [1..size]
        | getDir == West  = zip (map f [size,size+size .. size^2]) [size,size+size .. size^2]
        | getDir == East  = zip (map f [1,size+1 .. (size^2-(size-1))]) [1,size+1 .. (size^2-(size-1))]
        | getDir == South = zip (map f [(size^2-(size-1)) .. (size^2)]) [(size^2-(size-1)) .. (size^2)]
               where size = truncate (sqrt(fromIntegral ((snd $ bounds $ fstTrip graphPT) + 1))) :: Int
                     f    = getWhatIsAtNode graphPT
getPEdge _ _ = undefined



-- | Globals
a'' :: GraphATuple
a'' = graphTuple edgesForTestAGraph :: GraphATuple
b'' :: GraphPTuple
b'' = graphTuple edgesForTestPGraph :: GraphPTuple

a1' :: GraphATuple
a1' = graphTuple edgesForTestAGraph1' :: GraphATuple
a2' :: GraphATuple
a2' = graphTuple edgesForTestAGraph2' :: GraphATuple
a3' :: GraphATuple
a3' = graphTuple edgesForTestAGraph3' :: GraphATuple
a4' :: GraphATuple
a4' = graphTuple edgesForTestAGraph4' :: GraphATuple

width :: Size
width = 3

-- | Generating empty ant Graph of size width
newAQuad :: Int -> GraphATuple
newAQuad gwidth = graphFromEdges $ zip3 (replicate (gwidth^2) Nothing) (keyList gwidth) (adjListForNewGraph gwidth) :: GraphATuple

newAQuad' :: GraphATuple
newAQuad' = graphFromEdges $ edgesForTestAGraph1' :: GraphATuple


-- | Gennerating empty pheremone Graph of size width
newPQuad :: Int -> GraphPTuple
newPQuad gwidth = graphFromEdges $ zip3 (replicate (gwidth^2) 1.0) (keyList gwidth) (adjListForNewGraph gwidth) :: GraphPTuple

