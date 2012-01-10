import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show,Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState,Code)

instance Functor (Map.Map k) where  
    fmap f (k, v) =  (k ,(f v))  

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
        case Map.lookup lockerNumber map of
                Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
                Just (state,code) -> if state /= Taken
                                             then Right code
                                             else Left $ "Locker " ++ show lockerNumber ++ " is taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]

infixr 5 :-:
data List a  = Empty | a :-: (List a) deriving (Show,Read,Eq,Ord)

--data List a  = Empty | a Cons { listHead :: a, listTail :: List a } deriving (Show,Read,Eq,Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
        | x == a = Node x left right
        | x < a  = Node a (treeInsert x left) right
        | x > a  = Node a left (treeInsert x right)

treeElem  :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
        | x == a = True
        | x < a  = treeElem x left
        | x > a  = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
        Red == Red = True
        Yellow == Yellow = True
        Green == Green = True
        _ == _ = False

instance Show TrafficLight where
        show Red = "Red Light"
        show Yellow = "Yellow Light"
        show Green = "Green Light"

class Yesno a where
        yesno :: a -> Bool

instance Yesno Int where
        yesno 0 = False
        yesno _ = True

instance Yesno Bool where
        yesno = id

instance Yesno [a] where
        yesno [] = False
        yesno _  = True

instance Yesno TrafficLight where
        yesno Red = False
        yesno _   = True

yesnoIf :: (Yesno c) => c -> a -> a -> a
yesnoIf yesnoVal valIfYes valIfNo = if yesno yesnoVal then valIfYes else valIfNo 

instance Functor Tree where
        fmap f EmptyTree = EmptyTree
        fmap f (Node a leftNode rightNode) = Node (f a) (fmap f leftNode)  (fmap f rightNode)




