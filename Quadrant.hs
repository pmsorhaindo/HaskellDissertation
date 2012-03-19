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
import SimDefine

-- | Type synonym for the 3 tuple Ant Graph to make type signatures easier to read
type GraphATuple = (Array Vertex [Vertex], Int -> (Maybe Ant, Point, [Point]), Point -> Maybe Vertex)

-- | Similar to GraphATuple but holding pheremone levels instead of Ants.
type GraphPTuple = (Graph, Vertex -> (Double, Int, [Int]), Int -> Maybe Vertex)

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
edgesForTestAGraph = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Just(Ant 1 West 1.0 0 Return [West]),6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Just(Ant 1 West 0.2 0 Return [North]),9,[8,6])]

edgesForTestAGraph1' :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph1' = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestAGraph2' :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph2' = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Just(Ant 1 West 2.1  0 Return [South]),5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestAGraph3' :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph3' = [(Just(Ant 1 West 2.1  0 Return [East]),1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Nothing,6,[3,5,9]),(Nothing,7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestAGraph4' :: [(Maybe Ant, Int, [Int])]
edgesForTestAGraph4' = [(Nothing,1,[2,4]),(Nothing,2,[1,5,3]),(Nothing,3,[2,6]),(Nothing,4,[1,7,15]),(Nothing,5,[2,4,8,6]),(Just(Ant 1 West 2.1  0 Return [East]),6,[3,5,9]),(Just(Ant 1 West 2.1  0 Return [West]),7,[4,8]),(Nothing,8,[7,5,9]),(Nothing,9,[8,6])]

edgesForTestPGraph :: [(Double, Int, [Int])]
edgesForTestPGraph = [(0,1,[2,4]),(0,2,[1,5,3]),(0,3,[2,6]),(0,4,[1,7,15]),(0,5,[2,4,8,6]),(0,6,[3,5,9]),(0,7,[4,8]),(0,8,[7,5,9]),(0,9,[8,6])]


graphTuple :: [(node, Int, [Int])]                                      -- ^
        -> (Graph, Vertex -> (node, Int, [Int]), Int -> Maybe Vertex)   -- ^
graphTuple edgs    = graphFromEdges edgs

graph edgs         = fstTrip $ graphFromEdges edgs
graphfunc edgs     = sndTrip $ graphFromEdges edgs
graphfuncVert edgs = trdTrip $ graphFromEdges edgs

--list splitting functions take the first and second half, repsectively , of the list y when split at x.
splittedVertlist1 x y = fst $ splitAt x y
splittedVertlist2 x y = snd $ splitAt x y

swapNodes :: Int        -- ^
        -> Int          -- ^
        -> [a]          -- ^
        -> [a]          -- ^
-- huge bummer finding this bug... x < y or fail
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
adjVertsFromCombo :: (Graph, Vertex -> (t, t1, b), t2)  -- ^
        -> [b]                                          -- ^
adjVertsFromCombo z = map trdTrip (brokenUpGraph z)

updateGraph :: Int      -- ^
        -> Int          -- ^
        -> GraphATuple  -- ^
        -> GraphATuple  -- ^
updateGraph x y z = graphFromEdges $ zip3 (swapNodes x y (listOfNodes $ brokenUpGraph z)) ([1..])  (adjVertsFromCombo z)

--If it connects
--updateGraph':: Int-> Int-> (Graph, Int -> (node, Int, [Int]), t)-> [(node, Vertex, [Int])]
-- now changed to return Graph Tuple
updateGraph' :: Int             -- ^
        -> Int                  -- ^
        -> GraphATuple          -- ^
        -> Maybe GraphATuple    -- ^
updateGraph' x y z
        | y `elem` (legalEdges z $ x-1) = Just (graphFromEdges $ zip3 (swapNodes x y (listOfNodes $ brokenUpGraph z)) ([1..])  (adjVertsFromCombo z))
        | otherwise                     = Nothing

-- legalEdges
legalEdges :: GraphATuple       -- ^
        -> Int                  -- ^
        -> [Int]                -- ^
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

-- | Effectively maps a function across the graph. printing Each Graph as it goes.
eachSuccNode :: GraphATuple     -- ^
        -> Int                  -- ^
        -> IO ()                -- ^
eachSuccNode graphT iterator = do
                                 putStr $ (show origGraph) ++ "\n" ++ (show $ brokenUpGraph newGraph) ++ "\n"
                                 nxt <- eachSuccNode newGraph (iterator+1)
                                 putStr "Next!"
                                 where
                                    origGraph = brokenUpGraph graphT
                                    newGraph  = updateGraph iterator (iterator+1) graphT -- swaps the nodes with the next node (currently goes out of bounds)

-- Adds an Ant to a node (pos) of a given graph (graphT)
addAnt :: GraphATuple   -- ^
        -> Int          -- ^
        -> GraphATuple  -- ^
addAnt graphT pos = graphFromEdges $ zip3 (preList ++ [Just(Ant 1 East 1 0 Return ([]::[Direction]))] ++ sufList) ([1..]) (adjVertsFromCombo graphT)
        where preList | pos < 1 = []
                      | otherwise = take (pos-1) (listOfNodes $ brokenUpGraph graphT)
              sufList = drop pos (listOfNodes $ brokenUpGraph graphT)
--Allows a prexisting Ant to be placed in a graph.

addExistingAnt :: GraphATuple   -- ^
        -> Int                  -- ^
        -> Maybe Ant            -- ^
        -> GraphATuple          -- ^
addExistingAnt graphT pos passedAnt = graphFromEdges $ zip3 (preList ++ [passedAnt] ++ sufList) ([1..]) (adjVertsFromCombo graphT)
        where preList | pos < 1 = []
                      | otherwise = take (pos-1) (listOfNodes $ brokenUpGraph graphT)
              sufList = drop pos (listOfNodes $ brokenUpGraph graphT)

--list Of nodes that need Processing
listOfNodesWithAntsIn :: Eq a => (Graph, Vertex -> (Maybe a, t1, t2), t)    -- ^
        -> [t1]                                                             -- ^
listOfNodesWithAntsIn graphT = [vert | (ant,vert,_) <-xs , ant /= Nothing ] 
                        where xs = brokenUpGraph graphT 

--Proccess Ants at a listOfNodes - listOfNodesWithAntsIn can be used to make sure no Maybe's fall into the fromJust func.
--NEEDS TO BE A FOLD!!!
processAntsInGraph :: GraphPTuple       -- ^
        -> GraphATuple                  -- ^
        -> [Int]                        -- ^
        -> GraphATuple                  -- ^
processAntsInGraph graphPT graphAT procList = foldr (procAntAtNode graphPT) graphAT procList

--processEdgeAntsGraph :: GraphPTuple -> GraphATuple -> [EdgeAntsToProcess] -> [AdditionalPherInfo] ->[nodeAvailability] -> GraphATuple
processEdgeAntsGraph graphPT graphAT procList extraPher ndAvail = foldr (procAntAtNode graphPT) graphAT procList

--Once an Ant is known to be at a Node it can be extracted with this function.
--getAntFromNode :: Num a => (t, a -> (Maybe t2, t3, t4), t1) -> a -> (Ant, Int, [Int])
getAntFromNode graphT nd = (ant,key,adjList)
                        where (Just ant,key,adjList) = sndTrip graphT $ (nd-1) -- node-1 = Vertex

getWhatIsAtNode graphT nd = fstTrip $ sndTrip graphT $ (nd-1)

isAntAtNode :: GraphATuple      -- ^
        -> Int                  -- ^
        -> Bool                 -- ^
isAntAtNode graphT nd =  not $ isNothing presence 
                        where  (presence,_,_) = sndTrip graphT $ (nd-1)

-- Calculates the Maybe Target Node
calcTargetNode :: Size          -- ^
        -> (Ant, Point, t)      -- ^
        -> [Int]                -- ^
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

moveAnt :: GraphATuple  -- ^
        -> Int          -- ^
        -> GraphATuple  -- ^
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

procAntAtNode :: GraphPTuple    -- ^
        -> Int                  -- ^
        -> GraphATuple          -- ^
        -> GraphATuple          -- ^
procAntAtNode graphPT nd graphAT = do 
                           let pherLevels = senseSur graphPT nd -- still needed for recalculation of Dir.. ect
                           let newDir = makeDecision pherLevels -- DONE
                           let graphAT' = setDir graphAT nd newDir
                           let graphT2 = moveAnt graphAT' nd
                           graphT2

senseSur :: GraphPTuple         -- ^
        -> Int                  -- ^
        -> [(Direction,Double)] -- ^
senseSur graphT nd = map directionize (adjListForVertex (truncate (sqrt(fromIntegral(snd $ bounds $ fstTrip graphT)+1))) nd)
                where directionize x
                                | x == nd+1 = (East, (fstTrip $ (sndTrip graphT) (x-1))) -- Sort out function composition to make neater.
                                | x == nd-1 = (West, (fstTrip $ (sndTrip graphT) (x-1)))
                                | x > nd = (South, (fstTrip $ (sndTrip graphT) (x-1)))
                                | x < nd = (North, (fstTrip $ (sndTrip graphT) (x-1)))
                                | otherwise = undefined

-- | increaseSense --TODO
increaseSense :: [(Direction,Double)]   -- ^
        -> [(Direction,Double)]         -- ^
increaseSense = undefined



-- | Arranges the Pheremone list to hold
makeDecision :: [(Direction,Double)]    -- ^
        -> Direction                    -- ^
makeDecision pLevels = fst (maximumBy highestPher pLevels)
                where highestPher x y = (snd x) `compare` (snd y) 

-- |
makeDecisions :: [(Direction,Double)]   -- ^
        -> [(Direction,Double)]         -- ^
makeDecisions pLevels = sortBy highestPher pLevels
                where highestPher x y = (snd x) `compare` (snd y) 

-- | Change an Ants current direction at a given node to a given direction.
setDir :: GraphATuple   -- ^
        -> Point        -- ^
        -> Direction    -- ^
        -> GraphATuple  -- ^
setDir graphT nd newDir = addExistingAnt graphT nd (modif $ fstTrip $ getAntFromNode graphT nd) -- Not to be called it there isn't an ant at the node getAntFromNode will have a fit.
                where modif x = Just (Ant (antId x) newDir (pherLevel x) (age x) (aim x) (newDir:AntRepresent.path x)) 

-- | Function to process the whole Quadrant.
processAQuadrant :: GraphATuple -- ^
        -> GraphPTuple          -- ^
        -> GraphATuple          -- ^
processAQuadrant graphAT graphPT = do    
        let nodesToProcess = listOfNodesWithAntsIn graphAT
        let graphAT' = processAntsInGraph graphPT graphAT nodesToProcess
        graphAT'

-- |
processAQuadrant_ :: (GraphATuple, GraphPTuple) -- ^
        -> GraphATuple                          -- ^
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
getPEdge :: forall t1 t2 a e t3 a1. (Ix a, Integral a) => (Array a e, Int -> (a1, t1, t2), t3) -- ^
        -> Direction    -- ^
        -> [(a1, Int)] -- ^
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
newAQuad :: Int ->      -- ^
        GraphATuple     -- ^
newAQuad gwidth = graphFromEdges $ zip3 (replicate (gwidth^2) Nothing) (keyList gwidth) (adjListForNewGraph gwidth) :: GraphATuple

newAQuad' :: GraphATuple -- ^
newAQuad' = graphFromEdges $ edgesForTestAGraph1' :: GraphATuple


-- | Gennerating empty pheremone Graph of size width
newPQuad :: Int         -- ^
        -> GraphPTuple  -- ^
newPQuad gwidth = graphFromEdges $ zip3 (replicate (gwidth^2) 1.0) (keyList gwidth) (adjListForNewGraph gwidth) :: GraphPTuple

