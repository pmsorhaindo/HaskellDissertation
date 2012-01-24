addOne Nothing = Nothing
addOne (Just x) = Just (x+1)

ofmap f Nothing = Nothing
ofmap f (Just x) = Just (f x)
