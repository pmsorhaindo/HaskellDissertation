--My awesome (however not perfect) solution to Last but one problem
lastButOne :: [a] -> a
lastButOne xs = if length xs == 2
        then head xs
        else lastButOne (tail xs)

--type synonyms
type ReviewBody = String
type CustomerID = Int

data BookInfo = Book Int String [String]
        deriving (Show)

--defining data types Type Constructor = Value Constructor +[Data Components]
data MagazineInfo = Magazine Int String [String]
        deriving (Show)

data BookReview = BookReview BookInfo CustomerID String
        deriving (Show)
--data type with type synonyms for clarity
data BetterBookReview = BetterBookReview BookInfo CustomerID ReviewBody

--using a user defined datatype (notice the value constructor being used like a function.
myInfo = Book 100023 "The last time you laugh" ["Yo Momma", "Yo Poppa"]

--more type synonyms
type CardHolder = String
type CardNumber = String
type Address = [String]

-- An algebraic data type.. provide alternative type constructors. using |
data BillingInfo = CreditCard CardNumber CardHolder Address
        | CashOnDelievery
        | Invoice CustomerID
         deriving (Show)

bookID (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors

myNot True = False
myNot False = True

nicerID (Book id _ _) = id
nicerTitle (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

--Record Syntax.. save us having to write out the accessor methods above!

data Customer = Customer {
         customerID :: CustomerID
        ,customerName :: String
        ,customerAddress :: Address
        }deriving (Show)

-- Data can also be created using this syntax
customer1 = Customer{
         customerID = 1223
        ,customerAddress = ["Brighton","England"]
        ,customerName = "Jane Challenger Gillit"
        }
cite = Book 173 "Use of Weapons" ["Iain M. Banks"]
-- records get printed out differently to standard data assigns.
--Customer {customerID = 1223, customerName = "Jane Challenger Gillit", customerAddress = ["Brighton","England"]}
--Book 173 "Use of Weapons" ["Iain M. Banks"]

--Parameterised types

--data Maybe a = Just a
--        | Nothing

someBool = Just True

someString = Just "something"

badExample (x:xs) = x + badExample xs
badExample _ = 0

-- Parameterized Data Types? (what is it good for...)
data MaybeFS a = Float a
	| String a
	| Nothing a --Main.Nothing?
	deriving (Show)

--Recursive Data Types
data CustomList a = Cons a (CustomList a)
	|Nil
	deriving (Show)

--from List a class to take a list into a CustomList to prove I've mad a valid list.
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- Proof it works!!
--ghci>let x = fromList "durian"
--ghci>x
--Cons 'd' (Cons 'u' (Cons 'r' (Cons 'i' (Cons 'a' (Cons 'n' Nil)))))
--ghci>:t 
--x :: CustomList Char


--return to List Attempt
toList (Cons a as) = a : toList as
toList Nil = [] -- Good (assisted

-- Tree with Maybe
data MaybeTree a = MaybeNode a (Maybe (MaybeTree a)) (Maybe (MaybeTree a)) deriving (Show)
--Cracked it on my own but.. didn't realise to create trees I needed "Just"
--like => let a = MaybeNode "parent" Prelude.Nothing (Just (MaybeNode "right child" Prelude.Nothing Prelude.Nothing))
