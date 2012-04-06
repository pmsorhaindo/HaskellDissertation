module Food where
--Food

import SimDefine

data Flavour = Worm | MeatFragment | Termite | Leech
        deriving(Show)

data Food = Food {
        flavour :: Flavour,
        quantity :: Int
        }
        deriving(Show)

harvestFood :: Food -> Food
harvestFood food  = Food (flavour food) (quantity food-1)

