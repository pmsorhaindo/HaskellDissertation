{-# OPTIONS -XRankNTypes -XFlexibleContexts #-}
module QuadStitching where

import Data.Maybe(isJust,isNothing,fromJust)
import Data.Bool.HT

import AntRepresent
import SimDefine
import Quadrant
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

data WhichSide = Side | OtherSide

getSide :: Bool -> WhichSide -> (a,a) -> a
getSide True Side = fst
getSide False Side = snd
getSide True OtherSide = snd
getSide False OtherSide = fst

-- | A the front of a recursive group of functions which run through the list of Ants along both edges of connected Ant Quadrants. Each Ant is given the information and opportunity to move. If and Ant moves its added to the No process list to avoid ants being given the oppotunity to move twice. 
procEdgeAntAtNode ::  Int   -- ^ Start with 0 the index position into the list of  Maybe (Edge) Ants  being stitched together.
        -> StitchableQuads  -- ^ The initial Stitched up Quadrants (pre processing)                   
        -> StitchableQuads  -- ^ Returns the final Stitchedup Quads once each Ant has had the chance to move.
procEdgeAntAtNode pos qs  = do 
        -- rename fst and snd to adj and curr to make more readable?
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
                then antScenarios qs pos currAnt adjAnt
                else qs

-- | 
antScenarios :: StitchableQuads  -- ^
        -> Int                   -- ^
        -> Maybe Ant             -- ^
        -> Maybe Ant             -- ^
        -> StitchableQuads       -- ^
antScenarios qs pos currAnt adjAnt
                      | (isNothing currAnt) && (isNothing adjAnt) = procEdgeAntAtNode (1+pos) qs
                      | (isJust currAnt) && (isNothing adjAnt)    = loneEdgeAnt qs True pos
                      | (isNothing currAnt) && (isJust adjAnt)    = loneEdgeAnt qs False pos
                      | (isJust currAnt) && (isJust adjAnt)       = doubleEdgeAnt qs pos
antScenarios _ _ _ _ = undefined


-- | Lone Edge Ant
loneEdgeAnt :: StitchableQuads
        -> Bool
        -> Int
        -> StitchableQuads
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

        let newQs = loneMoveIt qs moveOutDir nd decisions isCurr --swapNode --addToNoProc

        --procEdgeAntAtNode (pos+1) qs
        newQs

-- | 
loneMoveIt  :: StitchableQuads          -- ^
        -> Direction                    -- ^
        -> Int                          -- ^
        -> [(Direction, Double)]        -- ^
        -> Bool                         -- ^
        -> StitchableQuads              -- ^
loneMoveIt qs modir nd (dec:decs) isCurr = loneMoveIt' qs modir nd (dec:decs) isCurr
loneMoveIt qs _ _  [] _ = qs -- No possible moves.


loneMoveIt' :: StitchableQuads          -- ^
        -> Direction                    -- ^
        -> Int                          -- ^
        -> [(Direction, Double)]        -- ^
        -> Bool                         -- ^
        -> StitchableQuads              -- ^
loneMoveIt' qs modir nd (dec:decs) isCurr | modir == fst dec = checkForAntOut qs modir nd (dec:decs) isCurr
                                               | otherwise = checkForAntIn qs modir nd (dec:decs) isCurr
loneMoveIt' _ _ _ [] _ = undefined

-- | This function checks to see if it is possible for an ant to move back from the edge into the quadrant.
checkForAntIn ::StitchableQuads         -- ^
        -> Direction                    -- ^
        -> Int                          -- ^
        -> [(Direction, Double)]        -- ^
        -> Bool                         -- ^
        -> StitchableQuads              -- ^
checkForAntIn qs modir nd (dec:decs) isCurr = do
        let nxtNd = nextNode nd (fst dec) $ qSize qs
        if isAntAtNode (fst$antGraphs qs) nxtNd
                then loneMoveIt qs modir nd decs isCurr
                else swapIn qs nd nxtNd (fst dec) isCurr

-- | This function moves ants back from the edge of quadrants into the same quadrant.
swapIn :: StitchableQuads       -- ^ The stitchable Quad which is to be processed.
        -> Int                  -- ^ First node supplied to update Graph. (Should contain an ant)
        -> Int                  -- ^ Second node supplied to update Graph. (Should contain Nothing)
        -> Direction            -- ^ The new direction for the ants movement.
        -> Bool                 -- ^ Switch to indicate which side of the stitchableQuad is being processed
        -> StitchableQuads      -- ^
swapIn qs nd1 nd2 d s  = newQs qs
        where newQs x     = StitchableQuads (quadSize x) (rel x) (ags x side) (pgs x) (aep x) (pep x) (npl x)
              side        = getSide s Side
              quadSize x  = (qSize qs)
              rel x       = (relation qs)
              ags x gs    = do
                             let di = gs (((setDir (fst$antGraphs qs) nd1 d),(snd$antGraphs qs)),
                                    ((fst$antGraphs qs),(setDir (snd$antGraphs qs) nd1 d)))
                             let gr = gs (((updateGraph nd1 nd2 (fst$di)),(snd$di)),
                                    ((fst$di),((updateGraph nd1 nd2 (snd$di)))))
                             gr
              --depending on which side the Ant is in a particular solution is chosen
              pgs x = (pherGraphs qs)
              aep x = (aEdgePair qs)
              pep x = (pEdgePair qs)
              npl x = side (((nd2: (fst$noProcList qs),snd$noProcList qs)),(fst$noProcList qs,nd2: (snd$noProcList qs)))

-- | This function checks to see if it is possible for the ant can move out of the quadrant
checkForAntOut :: StitchableQuads       -- ^
        -> Direction                    -- ^
        -> Int                          -- ^
        -> [(Direction, Double)]        -- ^
        -> Bool                         -- ^
        -> StitchableQuads              -- ^
checkForAntOut qs modir nd (dec:decs) isCurr = do 
        let outNd = outNode nd (fst dec) $ qSize qs
        if isAntAtNode (fst$antGraphs qs) outNd
                then loneMoveIt qs modir nd decs isCurr
                else swapOut qs nd outNd isCurr
checkForAntOut _ _ _ [] _ = undefined

-- | This function moves an Ant out of the quadrant to the adjacent quadrant in the Stitchable Quadrant structure.
swapOut :: forall t1 t2 t t3. StitchableQuads   -- ^
        -> Int                                  -- ^
        -> Int                                  -- ^
        -> Bool                                 -- ^
        -> StitchableQuads                      -- ^
swapOut qs currNd oppNd s = newQs qs
        where newQs x     = StitchableQuads (quadSize x) (rel x) (ags x side) (pgs x) (aep x) (pep x) (npl x)
              side        = getSide s Side
              quadSize x  = qSize qs
              rel x       = relation qs
              tempAnt     = Just (fstTrip (getAntFromNode (side (antGraphs qs)) currNd)) 
              -- actually doesn't because its only one Ant depending on which side the Ant is in
              -- a particular solution is chosen from the tuple.                    
              ags x gs   = gs(((addExistingAnt (fst$antGraphs qs) currNd Nothing),(addExistingAnt (snd$antGraphs qs) oppNd tempAnt)),(((addExistingAnt (fst$antGraphs qs) currNd tempAnt)),((addExistingAnt (snd$antGraphs qs) currNd Nothing))))
              pgs x      = pherGraphs qs
              aep x      = aEdgePair qs
              pep x      = pEdgePair qs
              npl x      = side ((fst$noProcList qs,oppNd:(snd$noProcList qs)),(oppNd: (fst$noProcList qs),snd$noProcList qs))
 


-- Y U NO WORK FOR ME :( - found that work around tho :D
otherSide :: forall a b. Eq ((a, a) -> a) => ((a, a) -> a) -> (b, b) -> b
otherSide side | side == fst = snd
               | side == snd = fst

-- | Getting the node value of the node in a given direction on the same graph
nextNode :: Int         -- ^
        -> Direction    -- ^
        -> Int          -- ^
        -> Int          -- ^
nextNode nd dir siz | dir == North = nd - siz
                    | dir == South = nd + siz
                    | dir == East = nd + 1
                    | dir == West = nd - 1

-- | Getting the node value of the node in a given direction on the other graph in a StitchableQuad structure  given an edge Node.
outNode :: Int          -- ^
        -> Direction    -- ^
        -> Int          -- ^
        -> Int          -- ^
outNode nd dir siz | dir == North = nd + (siz^2 - siz)
                   | dir == South = nd - (siz^2 - siz)
                   | dir == East = nd - (siz-1)
                   | dir == West = nd + (siz-1)

-- |
nodeFromEdgeIndex :: ((Direction, Direction)-> Direction)  -- ^
        -> StitchableQuads                                 -- ^
        -> Int                                             -- ^
        -> Int                                             -- ^
        -- TODO Faulty function detected
nodeFromEdgeIndex side qs pos | (side $ relation qs) == North = (pos+1) -- edgepair index to node.
                              | (side $ relation qs) == South = (pos+1) +((qSize qs)^2 - (qSize qs))
                              | (side $ relation qs) == East = (qSize qs) * (pos+1)
                              | (side $ relation qs) == West = (pos+1) * (qSize qs)

-- | Double Edge Ant
doubleEdgeAnt :: StitchableQuads        -- ^
        -> Int                          -- ^
        -> StitchableQuads              -- ^
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
        let moveOutDir2 = snd $ relation qs 

        let newQs =  doubleMoveIt qs (moveOutDir1,moveOutDir2) (nd1,nd2) (decs1,decs2) (False,False) :: StitchableQuads

        procEdgeAntAtNode (pos+1) newQs


-- | 
doubleMoveIt   :: StitchableQuads                         -- ^
        -> (Direction, Direction)                         -- ^
        -> (Int,Int)                                      -- ^
        -> ([(Direction, Double)], [(Direction, Double)]) -- ^
        -> (Bool, Bool)                                   -- ^
        -> StitchableQuads                                -- ^
doubleMoveIt qs modir nd (dec1:decs1,dec2:decs2) (False,False) = do
        let attemptMove = select  (doubleMoveIt qs modir nd (dec1:decs1,decs2) (False,False))  $
                --"attempt to Move Ant1 back in"
                (fst dec1 == oppDir(fst modir)  ,loneMoveIt qs (fst modir)  (fst nd) (dec1:decs1) True  ): 
                --"attempt to Move Ant2 back in"
                (fst dec2 == oppDir(snd modir)  ,loneMoveIt qs (snd modir)  (snd nd) (dec2:decs2) False):
                --"attempt to Move Ant1 along Edge"
                (not (fst dec1 ==  (fst modir) ),loneMoveIt qs (fst modir)  (fst nd) (dec1:decs1) True  ):
                --"attempt to Move Ant2 along Edge"
                (not (fst dec2 ==  (snd modir) ),loneMoveIt qs (fst modir)  (snd nd) (dec2:decs2) False): 
                -- needs to be random which one to remove
                (True                        ,doubleMoveIt qs modir nd (dec1:decs1,decs2) (False,False)): 
                []
        attemptMove
doubleMoveIt _ _ _ _ _ = undefined




doubleMoveOne :: StitchableQuads        -- ^
        -> Direction                    -- ^
        -> Int                          -- ^
        -> [(Direction, Double)]        -- ^
        -> Bool                         -- ^
        -> StitchableQuads              -- ^
doubleMoveOne qs modir nd (dec:decs) isCurr = doubleMoveOne' qs modir nd (dec:decs) isCurr
doubleMoveOne qs _ _ [] _ = qs -- No possible moves.

doubleMoveOne' :: StitchableQuads       -- ^
        -> Direction                    -- ^
        -> Int                          -- ^
        -> [(Direction, Double)]        -- ^
        -> Bool                         -- ^
        -> StitchableQuads              -- ^
doubleMoveOne' qs modir nd (dec:decs) isCurr | modir == fst dec = checkForAntOut qs modir nd (dec:decs) isCurr
                                             | otherwise = checkForAntIn qs modir nd (dec:decs) isCurr
doubleMoveOne' _ _ _ [] _ = undefined

