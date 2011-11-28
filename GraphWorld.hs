module GridWorld where
import Data.Graph
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
type Walkable = Int -- Enum so types of surface are more obvious.
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

theWorld :: [[([Maybe Ant], Maybe Food)]]

1  - 2  - 3  - 4  - 5  - 6
|    |    |    |    |    |
7  - 8  - 9  - 10 - 11 - 12
|    |    |    |    |    |
13 - 14 - 15 - 16 - 17 - 18
|    |    |    |    |    |
19 - 20 - 21 - 22 - 23 - 24
|    |    |    |    |    |
25 - 26 - 27 - 28 - 29 - 30
|    |    |    |    |    |
31 - 32 - 33 - 34 - 35 - 36

1[2,7]
2[1,8,3]
3[1,4,9]
4[3,10,5]
5[4,11,6]
6[5,12]
7[1,8,13]
8[2,7,9,14]
9[3,8,10,15]
10[4,9,11,16]
11[5,10,12,17]
12[6,11,18]
13[7,14,19]
14[8,13,15,20]
15[9,14,16,21]
16[10,15,17,22]
17[11,16,18,23]
18[12,17,24]
19[13,20,25]
20[14,19,21,26]
21[15,20,22,27]
22[16,21,23,28]
23[17,22,24,29]
24[18,23,30]
25[19,26,31]
26[20,25,27,32]
27[21,26,28,33]
28[22,27,29,34]
29[23,28,30,35]
30[24,29,36]
31[25,32]
32[26,31,33]
33[27,32,34]
34[28,33,35]
35[29,34,36]
36[30,35]

data NewRow = NewRow [(Integer,Integer)]
	deriving(Show)

data NewWorld = NewWorld [NewRow]
	deriving(Show)

returnWhereVar :: Int -> Int
returnWhereVar a = b
        where b = 10

listComp world = [a | Square (a,b,c,d) <- world, (get_xpos d)==0]
 

-- [ [ 2*x | x <- [1..y] ] | y <- [1,2,3] ] <-double lists comprehension within list comprehension which I ditched in favour of simple world
returnAtPos :: Int -> Int -> [Square] -> Square
returnAtPos px py world = Square (dataA,dataB,dataC, Position px py)
        where dataA = 0 + (head [a | Square (a,b,c,d) <- world, (get_xpos d)==px, (get_ypos d)==py ]) -- 0 + is a hack to get it to "evaluate"/ not complain that its trying to show a function
              dataB = 0 + (head [b | Square (a,b,c,d) <- world, (get_xpos d)==px, (get_ypos d)==py ])
              dataC = 0 + (head [c | Square (a,b,c,d) <- world, (get_xpos d)==px, (get_ypos d)==py ])




