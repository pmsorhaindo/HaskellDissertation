module Surface where
--surface

import SimDefine

data Surface = Wet Location | StableDry Location | UnstableDry Location
        deriving (Show)
