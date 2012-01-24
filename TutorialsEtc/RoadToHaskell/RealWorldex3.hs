mySecond  :: [a] -> a
mySecond xs = if null (tail xs)
	then error "list too short"
	else head (tail xs)

maybeSecond :: [a] -> Maybe a
maybeSecond [] = Nothing
maybeSecond xs = if null (tail xs)
	then Nothing
	else Just (head (tail xs))


--local binds with LET and IN
lend amount balance = let reserve = 100
			  newBalance = balance - amount
		      in if newBalance < reserve
			  then Nothing
			  else Just newBalance

--shadowing
foo = let a = 1
	in let b = 2
		in a+b
bar = let x=1
	in((let x = "foo" in x),x) -- an example that the same name can be used in nesting but t is dodgey.

--Shadowing a functions parameter
quux a = let a="foo"
	in a ++ "eek!"

--The parameter is basically ignored.
--ghci>quux 'a'
--"fooeek!"
--ghci>quux 3
--"fooeek!"
--ghci>quux True
--"fooeek!"

--local binds with WHERE
whereLend amount balance = if amount < reserve*0.5
                           then Just newBalance
                           else Nothing
    where newBalance = balance - amount
          reserve = 100


-- pasted from realworldhaskell cause I couldn't get my offsides(whitespace) right.... tabs suck -.-
lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve    = 100
          newBalance = balance - amount

-- defining local functions here we use the local function plural in where (special functionality for 0 and 1
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
        where plural 0 = "no " ++ word ++ "s"
              plural 1 = "one " ++ word
              plural n = show n ++ " " ++ word ++ "s"



whitespaceSucks = let a = 10
        --a comment doesn't count 
                      b = 20
        in if a==11
                then 10
                else 20
-- not a and b are in line!

-- defined in Data.Maybe fromMaybe
-- Was confused, it takes two parameters the defVal is a default value to return if the Maybe contains Nothing! :)
fromMaybe defval wrapped = 
        case wrapped of
                Nothing -> defval
                Just value -> value

caseExample x = case x of
        "value a" -> 10
        "value b" -> 20
        _ -> 0

-- Tree with Maybe
data MaybeTree a = MaybeNode a (Maybe (MaybeTree a)) (Maybe (MaybeTree a)) deriving (Show)
--Cracked it on my own but.. didn't realise to create trees I needed "Just"
--like => let a = MaybeNode "parent" Prelude.Nothing (Just (MaybeNode "right child" Prelude.Nothing Prelude.Nothing))

--pattern matches are overlapped?
--bad_nodesAreSame (MaybeNode a _ _) (MaybeNode b _ _) = Just a
--bad_nodesAreSame _ _ = Nothing

bad_nodesAreSame (MaybeNode a _ _) (MaybeNode b _ _)
	| a == b = Just a
bad_nodesAreSame _ _ = Nothing

lend3 amount balance
        | amount <= 0 = Nothing
        | amount > reserve * 0.5 = Nothing
        | True = Just newBalance
      where reserve = 100
            newBalance = balance - amount

