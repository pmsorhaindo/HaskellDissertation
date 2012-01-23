-- | This module provides an executable for the nonogram solver, able to parse
-- and solve nonograms
--
module Main
    ( main
    ) where

import Nonogram

import Data.Char (isSpace)
import Control.Monad (forM_, when)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

-- | Parse a nonogram according to the format specified at
--
-- <http://www.comp.lancs.ac.uk/~ss/nonogram/fmt2>
--
-- An example file would be:
--
-- > width 4
-- > height 5
-- >
-- > rows
-- > 4
-- > 1,1
-- > 1,1
-- > 1,2
-- > 3
-- >
-- > columns
-- > 5
-- > 1,1
-- > 1,2
-- > 4
--
nonogram :: Maybe Int -- ^ Known height
         -> Maybe Int -- ^ Known width
         -> [Description] -- ^ Rows
         -> [Description] -- ^ Columns
         -> [String] -- ^ Lines
         -> ([Description], [Description]) -- ^ Puzzle
nonogram _ _ r c [] = (r, c)
nonogram h w r c (l : ls) = case words l of
    ["height", h'] -> nonogram (Just $ read h') w r c ls
    ["rows", h'] -> nonogram (Just $ read h') w r c ls
    ["width", w'] -> nonogram h (Just $ read w') r c ls
    ["columns", w'] -> nonogram h (Just $ read w') r c ls
    ["rows"] -> let (r', t) = descriptions height [] ls
                       in nonogram h w r' c t
    ["columns"] -> let (c', t) = descriptions width [] ls
                       in nonogram h w r c' t
    _ -> nonogram h w r c ls
  where
    Just width = w
    Just height = h

-- | Parse a given number of descriptions
--
descriptions :: Int -> [Description] -> [String] -> ([Description], [String])
descriptions 0 ds ls = (reverse ds, ls)
descriptions _ ds [] = (reverse ds, [])
descriptions n ds (l : ls) = case description l of
    [] -> descriptions n ds ls
    d -> descriptions (n - 1) (d : ds) ls
  where
    description = map read . split ',' . filter (not . isSpace)

-- | Split a string on a certain character
--
split :: Char -> String -> [String]
split delim string = case break (== delim) string of
    (x, []) -> if null x then [] else [x]
    (x, xs) -> x : split delim (tail xs)

-- | Read a nonogram from a string
--
readNonogram :: String -> ([Description], [Description])
readNonogram = nonogram Nothing Nothing [] [] . lines

-- | Simpler function to read a nonogram from a file
--
readNonogramFromFile :: FilePath -> IO ([Description], [Description])
readNonogramFromFile = fmap readNonogram . readFile

-- | Main function
--
main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \file -> do
        hPutStrLn stderr $ "Parsing " ++ file
        (rows, columns) <- readNonogramFromFile file
        hPutStrLn stderr $ "Solving " ++ file
        putNonogram $ parallelNonogram rows columns
    when (null args) $ do
        name <- getProgName
        putStrLn $ "Usage: " ++ name ++ " files"
        putStrLn ""
        putStrLn "Where files are nonogram puzzles in the format specified"
        putStrLn "here: <http://www.comp.lancs.ac.uk/~ss/nonogram/fmt2>"
