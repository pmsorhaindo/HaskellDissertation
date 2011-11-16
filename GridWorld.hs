module GridWorld where
--import qualified AntRepresent as Ant 

localSquare :: [[a]] -> (Int,Int) -> [[a]]
localSquare xss (x,y) = map (take y . drop n) $ (take x . drop n) xss
  where n = length xss `div` 2

matrix = replicate 10 [0..9]

data Position = Position (Int, Int)
	deriving(Show)
data Square = Square (Int,Int,Int,Position)
	deriving(Show)
data Row = Row [Square]
	deriving(Show)
data World = World [Row]
	deriving(Show)

mkSquare :: Int -> Int -> Int -> (Int, Int) -> Square
mkSquare x y z p = Square (x, y, z, Position p)

--sq1 :: Square
--sq1 = (mkSquare 0,0,0, (0,0))

antWorld :: World
--antWorld = World [ Row [mkSquare $ (0,0,0, (0,0))]]
antWorld = World [ Row [ Square (0,0,0, Position (0,0)), Square (0,0,0, Position (1,0)), Square (0,0,0, Position (2,0)), Square (0,0,0, Position (3,0)), Square (0,0,0, Position (4,0))]]
--	    	  [(0,0,0,(0,1)),(0,0,0,(1,1)),(0,0,0,(2,1)),(0,0,0,(3,1)),(0,0,0,(4,1))],
--	    	  [(0,0,0,(0,2)),(0,0,0,(1,2)),(0,0,0,(2,2)),(0,0,0,(3,2)),(0,0,0,(4,2))],
--	    	  [(0,0,0,(0,3)),(0,0,0,(1,3)),(0,0,0,(2,3)),(0,0,0,(3,3)),(0,0,0,(4,3))],
--	    	  [(0,0,0,(0,4)),(0,0,0,(1,4)),(0,0,0,(2,4)),(0,0,0,(3,4)),(0,0,0,(4,4))]]

data NewRow = NewRow [(Integer,Integer)]
	deriving(Show)

data NewWorld = NewWorld [NewRow]
	deriving(Show)
