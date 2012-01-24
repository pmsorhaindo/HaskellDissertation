import Data.Array
 
type Grid = Array Int Bool
 -- The grid is flattened into one dimension for simplicity.
 
life :: Int -> Int -> Grid -> Grid
{- Returns the given Grid advanced by one generation. -}
life w h old =
    listArray (bounds old) (map f coords)
  where coords = [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
 
        f (x, y) | c && (n == 2 || n == 3) = True
                 | not c && n == 3         = True
                 | otherwise               = False
          where c = get x y
                n = count [get (x + x') (y + y') |
                    x' <- [-1, 0, 1], y' <- [-1, 0, 1],
                    not (x' == 0 && y' == 0)]
 
        get x y | x < 0 || x == w = False
                | y < 0 || y == h = False
                | otherwise       = old ! (x + y*w)
 
count :: [Bool] -> Int
count []          = 0
count (False : l) = count l
count (True  : l) = 1 + count l


--Running
grid :: [String] -> (Int, Int, Grid)
grid l = (width, height, a)
  where (width, height) = (length $ head l, length l)
        a = listArray (0, width * height - 1) $ concatMap f l
        f = map g
        g '.' = False
        g _   = True
 
printGrid :: Int -> Grid -> IO ()
printGrid width = mapM_ f . split width . elems
  where f = putStrLn . map g
        g False = '.'
        g _     = '#'
 
split :: Int -> [a] -> [[a]]
split _ [] = []
split n l  = a : split n b
  where (a, b) = splitAt n l
 
blinker = grid
   [".#.",
    ".#.",
    ".#."]
 
glider = grid
   ["............",
    "............",
    "............",
    ".......###..",
    ".......#....",
    "........#...",
    "............"]
 
printLife :: Int -> (Int, Int, Grid) -> IO ()
printLife n (w, h, g) = mapM_ f $ take n $ iterate (life w h) g
  where f g = do
            putStrLn "------------------------------"
            printGrid w g
 
main = printLife 10 blinker
