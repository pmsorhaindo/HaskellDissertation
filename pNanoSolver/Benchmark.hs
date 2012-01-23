module Main
    ( main
    ) where

import Criterion.Main

import Nonogram

main :: IO ()
main = defaultMain
    [ bgroup "sequential" (map (bench' sequentialNonogram) puzzles)
    , bgroup "parallel" (map (bench' parallelNonogram) puzzles)
    ]
  where
    bench' f (name, rows, columns) =
        bench name $ nf (show . uncurry f) (rows, columns)

type Puzzle = (String, [[Int]], [[Int]])

puzzles :: [Puzzle]
puzzles = [puzzle_5x5, puzzle_10x10, puzzle_15x15, puzzle_20x20]

puzzle_5x5 :: Puzzle
puzzle_5x5 =
    ( "puzzle_5x5"
    , [[2, 1], [2, 2], [2, 1], [2], [1, 2]]
    , [[3], [3, 1], [1], [2, 2], [2, 1]]
    )
{-# NOINLINE puzzle_5x5 #-}

puzzle_10x10 :: Puzzle
puzzle_10x10 =
    ( "puzzle_10x10"
    , [[3, 1, 1], [5, 1, 1], [1, 1, 2], [1, 1, 1, 1, 1], [2, 2], [1, 5], [2, 3, 2], [1, 2, 1], [1, 3, 4], [1, 1]]
    , [[2, 1, 1, 2], [3, 1, 1], [2, 2, 1, 2], [1, 1, 1], [4, 2, 1], [3], [4], [2, 3, 1], [1, 1, 1, 1], [3, 4]]
    )
{-# NOINLINE puzzle_10x10 #-}

puzzle_15x15 :: Puzzle
puzzle_15x15 =
    ( "puzzle_15x15"
    , [[2, 1, 1, 1, 1], [2, 1, 3, 1], [2, 1, 1, 2, 2], [1, 6, 3], [6, 2, 1], [1, 3, 2], [1, 1, 1, 1, 1, 3], [1, 5, 1, 2, 1], [1, 2, 1, 5], [1, 1, 2, 1], [5, 3, 1], [1, 1, 1, 2, 1], [3, 1, 1], [2, 2, 1, 1], [6, 1, 1, 2]]
    , [[1, 1, 2, 1], [3, 1, 2], [1, 1, 1, 5, 2], [3, 1, 1, 1, 1], [2, 3, 1, 3], [5, 2, 5], [1, 3, 1, 1, 1], [6, 1, 1], [2, 1, 1, 4, 1], [1, 1, 1, 3, 1], [3, 2, 2], [3, 2, 1, 1], [1, 1, 2, 1], [1, 2, 1, 2], [1, 1, 1, 2, 3, 1]]
    )
{-# NOINLINE puzzle_15x15 #-}

puzzle_20x20 :: Puzzle
puzzle_20x20 =
    ( "puzzle_20x20"
    , [[3], [5], [3, 1], [2, 1], [3, 3, 4], [2, 2, 7], [6, 1, 1], [4, 2, 2], [1, 1], [3, 1], [6], [2, 7], [6, 3, 1], [1, 2, 2, 1, 1], [4, 1, 1, 3], [4, 2, 2], [3, 3, 1], [3, 3], [3], [2, 1]]
    , [[2], [1, 2], [2, 3], [2, 3], [3, 1, 1], [2, 1, 1], [1, 1, 1, 2, 2], [1, 1, 3, 1, 3], [2, 6, 4], [3, 3, 9, 1], [5, 3, 2], [3, 1, 2, 2], [2, 1, 7], [3, 3, 2], [2, 4], [2, 1, 2], [2, 2, 1], [2, 2], [1], [1]]
    )
{-# NOINLINE puzzle_20x20 #-}
