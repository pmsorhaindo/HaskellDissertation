module Food where
--Food

import SimDefine

data Food = Food {
        loc :: Location,
        quantity :: Int
        }
        deriving(Show)

harvestFood :: Food -> Food
harvestFood food  = Food (loc food) (quantity food-1)

