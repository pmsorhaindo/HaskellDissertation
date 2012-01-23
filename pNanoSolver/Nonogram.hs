--Parallel Trials

module Nonogram
        ( Description
         ,sequentialNonogram
         ,parallelNonogram
         ,putNonogram
         ) where

import Control.Monad (when, mplus, foldM)
import Control.Parallel (par, pseq)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data Colour = White | Black
        deriving (Show,Eq)

type Nonogram = [[Colour]]
type Description = [Int]

data Partial = MustBe Colour | BlackArea Int
        deriving (Show,Eq)

fromDescription :: Description -> [Partial]
fromDescription = map BlackArea

emptyPartial :: [Partial]-> Bool
emptyPartial [] = True
emptyPartial (MustBe Black : _) = False
emptyPartial (BlackArea _ : _ ) = False
emptyPartial (_ : xs) = emptyPartial xs

type Partials = IntMap [Partial]

learnCell :: Colour -> [Partial] -> Maybe [Partial]
learnCell White [] = Just []
learnCell Black [] = Nothing
learnCell y (MustBe x : ds) = if x == y then Just ds else Nothing
learnCell White ds = Just ds
learnCell Black(BlackArea n : ds) = Just $ replicate (n-1) (MustBe Black) ++ (MustBe White : ds)

learnCellAt :: Colour -> Partials -> Int -> Maybe Partials
learnCellAt cell partials index = do
        ds <- learnCell cell $ partials IM.! index
        return $ IM.insert index ds partials

type Branching a = Maybe a -> Maybe a -> Maybe a

sequential :: Branching a
sequential = mplus

parallel :: Branching a
parallel x y = x `par` y `pseq` mplus x y

solve :: Branching Nonogram -> Int -> Int -> [Description] -> Partials -> Maybe Nonogram
solve branch width = solve'
        where
            solve' column descriptions partials
                | null descriptions = 
                        if all emptyPartial (map snd $ IM.toList partials)
                                then return [[]]
                                else Nothing
                | null rd = do
                        ps <- foldM (learnCellAt White) partials [column .. width - 1]
                        rows <- solve' 0 rds ps
                        return $ replicate (width - column) White : rows
                | otherwise = branch place skip
                        where
                            (rd : rds) = descriptions
                            (l : ds) = rd
                            
                            place = do
                                when (column + l > width) Nothing
                                ps <- foldM (learnCellAt Black) partials
                                        [column .. column + l -1]
                                let atEnd = column + l == width
                                
                                ps' <- if atEnd then return ps
                                                else learnCellAt White ps (column +l)

                                (row : rows) <- solve' (column + l + 1) (ds : rds) ps'
                                let row' = if atEnd then row else White : row
                                return $ (replicate l Black ++ row') : rows

                            skip = do
                                when (column >= width) Nothing
                                ps <- learnCellAt White partials column
                                (row : rows) <- solve' (column + 1) ((l : ds) : rds) ps
                                return $ ((White : row)) : rows

nonogram :: Branching Nonogram -> [Description] -> [Description] -> Maybe Nonogram
nonogram branch rows columns = solve branch (length columns) 0 rows state
        where
            state = IM.fromList (zip [0 ..] $ map fromDescription columns)

sequentialNonogram :: [Description] -> [Description] -> Maybe Nonogram
sequentialNonogram = nonogram sequential

parallelNonogram :: [Description] -> [Description] -> Maybe Nonogram
parallelNonogram =  nonogram parallel

putNonogram :: Maybe Nonogram -> IO()
putNonogram Nothing = putStrLn "No solution found"
putNonogram (Just s) = mapM_ (putStrLn . concatMap showCell) s
        where
            showCell Black = "X"
            showCell White = "-"
