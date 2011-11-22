module GridWorld where
import qualified AntRepresent as Ant 

gridSize :: Int
gridSize = 10

localSquare :: [[a]] -> (Int,Int) -> [[a]]
localSquare xss (x,y) = map (take y . drop n) $ (take x . drop n) xss
  where n = length xss `div` 2

matrix = replicate 10 [0..9]

--data Position = Position (Int, Int)
--	deriving(Show)

type AmountOfAnts = Int
type Walkable = Int
type AmountOfFood = Int

data Position = Position{ get_xpos, get_ypos :: Int }
        deriving(Show)

data Square = Square (AmountOfAnts,Walkable,AmountOfFood,Position)
	deriving(Show)
data AntSquare = AntSquare (Ant.Ant)
        deriving(Show)


data Row = Row [Square]
	deriving(Show)
data World = World [Row]
	deriving(Show)
data SimpleWorld = SimpleWorld [Square]
        deriving(Show)


mkSquare :: Int -> Int -> Int -> Int -> Int -> Square
mkSquare x y z px py = Square (x, y, z, Position px py)

--sq1 :: Square
--sq1 = (mkSquare 0,0,0, (0,0))

antWorld :: World
--antWorld = World [ Row [mkSquare $ (0,0,0, (0,0))]]
antWorld = World [ Row [ Square (0,0,0, Position 0 0), Square (0,0,0, Position 1 0), Square (0,0,0, Position 2 0), Square (0,0,0, Position 3 0), Square (0,0,0, Position 4 0)],
	    	   Row [ Square (0,0,0, Position 0 1), Square (0,0,0, Position 1 1), Square (0,0,0, Position 2 1), Square (0,0,0, Position 3 1), Square (0,0,0, Position 4 1)],
	    	   Row [ Square (0,0,0, Position 0 2), Square (0,0,0, Position 1 2), Square (0,0,0, Position 2 2), Square (0,0,0, Position 3 2), Square (0,0,0, Position 4 2)],
	    	   Row [ Square (0,0,0, Position 0 3), Square (0,0,0, Position 1 3), Square (0,0,0, Position 2 3), Square (0,0,0, Position 3 3), Square (0,0,0, Position 4 3)],
	    	   Row [ Square (0,0,0, Position 0 4), Square (0,0,0, Position 1 4), Square (0,0,0, Position 2 4), Square (0,0,0, Position 3 4), Square (0,0,0, Position 4 4)]]

simpleAntWorld = [ Square (1,1,1, Position 0 0), Square (3,3,2, Position 1 0), Square (0,0,0, Position 2 0), Square (0,0,0, Position 3 0), Square (0,0,0, Position 4 0),
                   Square (1,2,1, Position 0 1), Square (1,0,1, Position 1 1), Square (0,0,0, Position 2 1), Square (0,0,0, Position 3 1), Square (0,0,0, Position 4 1),
                   Square (3,3,3, Position 0 2), Square (0,8,0, Position 1 2), Square (0,0,0, Position 2 2), Square (0,0,0, Position 3 2), Square (0,0,0, Position 4 2),
                   Square (6,6,6, Position 0 3), Square (8,0,8, Position 1 3), Square (0,0,0, Position 2 3), Square (0,0,0, Position 3 3), Square (0,0,0, Position 4 3),
                   Square (8,0,0, Position 0 4), Square (0,0,0, Position 1 4), Square (0,0,0, Position 2 4), Square (0,0,0, Position 3 4), Square (3,4,7, Position 4 4)] 

data NewRow = NewRow [(Integer,Integer)]
	deriving(Show)

data NewWorld = NewWorld [NewRow]
	deriving(Show)

returnWhereVar :: Int -> Int
returnWhereVar a = b
        where b = 10

listComp world = [a | Square (a,b,c,d) <- world, (get_xpos d)==0]
 

-- [ [ 2*x | x <- [1..y] ] | y <- [1,2,3] ] <-irc solution to lists comprehension within list comprehension which I ditched in favour of simple world
returnAtPos :: Int -> Int -> [Square] -> Square
returnAtPos px py world = Square (dataA,dataB,dataC, Position px py)
        where dataA = 0 + (head [a | Square (a,b,c,d) <- world, (get_xpos d)==px, (get_ypos d)==py ]) -- 0 + is a hack to get it to "evaluate"/ not complain that its trying to show a function
              dataB = 0 + (head [b | Square (a,b,c,d) <- world, (get_xpos d)==px, (get_ypos d)==py ])
              dataC = 0 + (head [c | Square (a,b,c,d) <- world, (get_xpos d)==px, (get_ypos d)==py ])




