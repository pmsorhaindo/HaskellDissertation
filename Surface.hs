module Surface where
--surface

import SimDefine

data Surface = Wet Location | StableDry Location | UnstableDry Location
        deriving (Show)

--generateInitialSurface :: (maxX,maxY,minX,minY)->[Surface]
--generateInitialSurface = genSurface minX minY 
{-let 
        genSurface x y
        | x==minX && y==minY             = StableDry (Location minX minY) : genSurface (x+1) y
        | x LT||EQ maxX && y LT||EQ maxY = StableDry (Location x y) : genSurface (x+1) y
        | x GT maxX && y LT maxY         = StableDry (Location minX (y+1)) : genSurface (minX+1) (y+1) -}
