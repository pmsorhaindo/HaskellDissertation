module Food where
--Food

import SimDefine

type Quantity = Int

data Food = Food Location Quantity
        deriving(Show)
