newtype Lover a = Lover { loverDiary :: (Name,a)}
        deriving (Show)
type Name = String

createLover name times = Lover (name, times)

startAnAffairWith name (Lover (names,times)) = Lover (name,0)

jenny = startAnAffairWith "Jenny "
rosie = startAnAffairWith "Rosie "
emm = startAnAffairWith "Emma "
bob = createLover "Paula " 5

oneMoreTime (Lover (name, times)) = Lover (name, times+1)

changeBeloved newname (Lover (name, times)) = Lover (name ++ newname, times)

chainAffairs (Lover (oldnames, oldtimes)) (Lover (newname,newtimes)) = Lover (oldnames ++ newname, oldtimes+newtimes)

times f (Lover (names,times)) = Lover (names, f times)

class Macho f where
        chain :: (Num a) => f a -> f a -> f a

instance Macho Lover where
        chain mychicks = chainAffairs mychicks

instance Functor Lover where
        fmap f = times f 


tellLover newtimes oldtimes = Lover ("", newtimes+oldtimes)

askLover lover answer = Lover (oldnames ++ newnames , newtimes) -- partially applied question retrievs the answer from tell Lover and old diary knowledge.
        where (oldnames, oldtimes) = loverDiary lover
              (newnames, newtimes)  = loverDiary (answer oldtimes)


tellMyself newtimes = Lover ("",newtimes) -- adding new times via talking to himself

newLove love = Lover (love,0) -- resetting Bob's diary


